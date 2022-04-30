;;; tree-view-mode.el --
;; Copyright (C) 2022 fubuki

;; Author:   fubuki@frill.org
;; Keywords: tools unix
;; Version:  @(#)$Revision: 1.42 $

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tree text display mode.

;;; Installation:

;; (require 'tree-view-mode)

;;; Code:
(require 'tree)

(defconst tree-view-mode-version "$Revision: 1.42 $")

(if (boundp 'tree-view-mode) (setq tree-view-mode 'tree-view-mode))

(defcustom tree-progress-border 9999
  "ディレクトリ数が数値以上なら進捗を表示する."
  :type  '(choice integer (const :tag "Non print" nil))
  :group 'tree)

;; work variables
(defvar tree-symbol 'tree-overlay)
(defvar tree-overlays-list nil "buffer local variable.")
(defvar tree-1st-line 1)
(defvar tree-directory-total 0)
(defvar tree-overlay-total 0)

(defun tree-text2list ()
  "Directory tree を表示したカレントバッファから \
ディレクトリの始点と終点を集め Tree 構造を維持したままリストにする."
  (let (branch name length beg result)
    (catch 'break
      (while (re-search-forward tree-branch-regexp nil t)
        (setq branch (match-string-no-properties 1)
              name   (match-string-no-properties 3) ; 未使用
              length (length branch)
              beg    (line-end-position))
        (when (< length (tree-text-next-branch-length))
          (let (tmp)
            (tree-add-properties (match-beginning 3) beg '(face tree-directory))
            (forward-line)
            (setq tmp (tree-text2list))
            (setq tree-directory-total (1+ tree-directory-total))
            (setq result (cons (cons (cons beg (car tmp)) (cdr tmp)) result))))
        (if (string-match tree-branch-term branch)
            (throw 'break (cons (line-end-position) (reverse result))))))))

(defun tree-pairp (elt)
  "ELT がドット対のコンスセルなら t."
  (and (consp elt)
       (atom (car elt))
       (and (cdr elt) (atom (cdr elt)))))

(defun tree-add-properties (beg end properties)
  "BEG END に PROPERTIES を追加する."
  (let ((ov (make-overlay beg end))
        pty val)
    (while properties
      (setq pty (nth 0 properties)
            val (nth 1 properties))
      (overlay-put ov pty val)
      (setq properties (cddr properties)))  
    ov))

(defun tree-make-overlap (lst)
  "LST を元に overlay をを生成し list で戻す."
  (let (result)
    (while lst
      (cond
       ((tree-pairp (car lst))
        (let* ((pair (car lst))
               (beg  (car pair))
               (end  (cdr pair))
               (sym  tree-symbol)
               (ov   (tree-add-properties
                      beg end
                      `(invisible ,sym
                                  ,sym ,sym
                                  isearch-open-invisible tree-hide-block-open))))
          (setq result (cons ov result))))
       ((consp (car lst))
        (setq tree-overlay-total (1+ tree-overlay-total))
        (setq result (append result (tree-make-overlap (car lst))))))
      (setq lst (cdr lst)))
    (and tree-progress-border (> tree-directory-total tree-progress-border)
         (let ((progress (floor (/ (* tree-overlay-total 100.0) tree-directory-total))))
           (and (zerop (% progress 5)) (message "make overlay...%d%%" progress))))
    (nreverse result)))

(defun tree-at-overlays ()
  (interactive)
  (let (result ovl)
    (setq ovl (get-char-property-and-overlay (point) tree-symbol))
    (dolist (ov (overlays-at (point) 'sorted))
      (setq result
            (cons (cons (overlay-start ov) (overlay-end ov))
                  result)))
    (message "%s (%s)" (reverse result) (cdr ovl))))

(defun tree-directory-line-p ()
  "現在位置が見かけ上の directory 行なら Directory名先頭位置を、さもなくば nil を返す."
  (let* (pos pty)
    (save-excursion
      (if (and (eolp) (not (invisible-p (point)))) (forward-char -1))
      (while (invisible-p (point)) (forward-char -1))
      (setq pos
            (next-single-char-property-change
             (line-beginning-position) 'face nil (line-end-position))
            pty (get-char-property pos 'face))
      (if (eq pty 'tree-directory) pos))))

(defun tree-include-directory-p (beg end)
  "BEG END 間にディレクトリが在れば non-nil."
  (not (eq end
           (next-single-char-property-change beg 'face nil end))))

(defun tree-position ()
  (let ((dp (tree-directory-line-p)))
    (if dp
        (save-excursion
          (goto-char dp)
          (line-end-position))
      (1- (line-end-position)))))

(defun tree-overlays-in (ov1 ov2)
  "overlay OV1 は overlay OV2 の範囲内に収まっているか?"
  (let ((beg1 (overlay-start ov1))
        (end1 (overlay-end ov1))
        (beg2 (overlay-start ov2))
        (end2 (overlay-end ov2)))
    (and (<= end1 end2) (>= beg1 beg2))))

;;;###autoload
(defun tree-point-filename (&optional full)
  "ポイントのファイル名を返す FULL が non-nil ならフルパス."
  (let* ((ov  (cdr (get-char-property-and-overlay (1- (line-end-position)) tree-symbol)))
         (ovs (and ov (tree-live-branch ov)))
         (path (if (or full (= tree-1st-line (line-number-at-pos))) tree-directory ""))
         name)
    (save-excursion
      (beginning-of-line)
      (and (looking-at tree-branch-regexp)
           (setq name (tree-quote-strip (match-string-no-properties 3))))
      (when full
        (dolist (a ovs)
          (let ((beg (overlay-start a)))
            (goto-char beg)
            (beginning-of-line)
            (looking-at tree-branch-regexp)
            (setq path
                  (concat path
                          (let ((tmp
                                 (tree-quote-strip (match-string-no-properties 3))))
                            (if (string= "/" (substring tmp -1))
                                tmp
                              (concat tmp "/"))))))))
      (setq path (concat path name)))
    path))

(defun tree-directory-location ()
  "バッファ 1行目を文字列として返す. 但し行頭がセミコロンの行はスキップされる.
その行数はバッファローカル変数`tree-1st-line' にセットされる."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at ";") (forward-line))
    (setq-local tree-1st-line (line-number-at-pos))
    (file-name-as-directory
     (buffer-substring-no-properties
      (point) (line-end-position)))))

;;;###autoload
(defun tree-point-invert ()
  "ポイントのディレクトリを折り畳み/展開する."
  (interactive)
  (let ((ov  (car (overlays-at (tree-position) 'sorted)))
        val)
    (if ov
        (if (setq val (or (overlay-get ov 'invisible)))
            (tree-point-show)
          (tree-point-hide))
      (tree-next-line))))
           
;;;###autoload ***
(defun tree-next-visit ()
  "閉じていれば 開いて中に入る.
(ディレクトリの無い層に居て)開いていれば閉じて次のディレクトリに行く."
  (interactive)
  (let ((dir (tree-directory-line-p))
        ov val)
    ;; Point adjust.
    (if dir
        (goto-char dir)
      (setq ov (cdr (get-char-property-and-overlay (point) tree-symbol)))
      (unless (and ov (tree-include-directory-p (point) (overlay-end ov)))
        (tree-point-hide (point)))
      (tree-next-directory))
    (end-of-line)
    (setq val (get-char-property (point) 'invisible))
    (if val
        (progn
          (tree-point-show)
          (tree-next-line))
      (tree-point-hide)
      (tree-next-directory))
    (recenter)))

;;;###autoload
(defun tree-previous-visit ()
  "開いていれば閉じてひとつ前のディレクトリに移動.
閉じていれば開いて中に降りる."
  (interactive)
  (let ((dir (tree-directory-line-p))
        val)
    (if dir
        (goto-char dir)
      (tree-previous-directory))
    (end-of-line)
    (setq val (get-char-property (point) 'invisible))
    (if val
        (progn
          (tree-point-show)
          (tree-next-line))
      (tree-point-hide)
      (tree-previous-directory))
    (recenter)))

;;;###autoload
(defun tree-point-hide (&optional pos)
  "POS とそれに絡むブロックすべてを hide."
  (interactive "P")
  (let* ((pos (if pos (prefix-numeric-value pos) (tree-position)))
         (ov  (cdr (get-char-property-and-overlay pos tree-symbol))))
    (dolist (o (overlays-at pos 'sorted))
      (and (overlay-get o tree-symbol)
           (tree-overlays-in o ov)
           (overlay-put o 'invisible tree-symbol)))
    (recenter)))

;;;###autoload
(defun tree-point-show (&optional pos)
  (interactive)
    (let* ((pos (or pos (tree-position)))
           (ov (cdr (get-char-property-and-overlay pos tree-symbol))))
      (and ov
           (overlay-get ov 'invisible)
           (overlay-put ov 'invisible nil))
    (recenter)))

;;;###autoload
(defun tree-hide-all ()
  "all hide."
  (interactive)
  (dolist (ov tree-overlays-list)
    (overlay-put ov 'invisible tree-symbol))
  (recenter))

;;;###autoload
(defun tree-show-all ()
  "all show."
  (interactive)
  (dolist (ov tree-overlays-list)
    (overlay-put ov 'invisible nil))
  (recenter))

;;;###autoload
(defun tree-hide-except-point ()
  "ポイントのあるブロック以外を閉じる."
  (interactive)
  (let* ((pos (tree-position))
         (ov (cdr (get-char-property-and-overlay pos tree-symbol))))
    (push-mark)
    (dolist (o tree-overlays-list)
        (if (tree-overlays-in ov o)
            (overlay-put o 'invisible nil)
          (overlay-put o 'invisible tree-symbol))))
    (recenter))

;;;###autoload
(defun tree-point-branch-show ()
  "ポイントのブロックから枝の伸びているブロックをすべて開く."
  (interactive)
  (let* ((pos (tree-position))
         (ov (cdr (get-char-property-and-overlay pos tree-symbol)))
         (end (overlay-end ov)))
    (dolist (o tree-overlays-list)
      (and (tree-overlays-in o ov)
           (overlay-put o 'invisible nil)))
    (recenter)))

;;;###autoload
(defun tree-copy-filename (&optional prefix)
  "ポイントのファイル名またはディレクトリ名を表示し kill-ring にコピー.
PREFIX 在りだとフルパスになる."
  (interactive "P")
  (let ((result (tree-point-filename prefix)))
    (and result (kill-new result) (message "%s" result))))

(defun tree-live-branch (ov)
  "OV が範囲内に収まる Overlay すべてを 'tree-overlays-list' から返す."
  (let (result)
    (dolist (o tree-overlays-list (reverse result))
      (if (tree-overlays-in ov o)
          (setq result (cons o result))))))

(defun tree-hide-block-open (dummy)
  ;; 何故だか DUMMY と OV が一致していないときがあるので別途取得.
  (let* ((ov (cdr (get-char-property-and-overlay (point) tree-symbol)))
         (ovs (tree-live-branch ov)))
    (dolist (o tree-overlays-list)
      (if (member o ovs)
          (overlay-put o 'invisible nil)
        (overlay-put o 'invisible tree-symbol)))
    (recenter)))

(defun tree-next-line (&optional arg)
  "ポイントを次の行のファイル名先頭に移動."
  (interactive "p")
  (let ((arg (prefix-numeric-value arg)))
    (dotimes (i arg)
      (while (progn
               (re-search-forward tree-branch-regexp nil t)
               (goto-char (match-beginning 3))
               (invisible-p (point)))))))

(defun tree-previous-line (&optional arg)
  "ポイントを前の行のファイル名先頭に移動."
  (interactive "p")
  (let ((arg (prefix-numeric-value arg)))
    (dotimes (i arg)
      (while (progn
               (beginning-of-line)
               (re-search-backward tree-branch-regexp nil t)
               (goto-char (match-beginning 3))
               (invisible-p (point)))))))

(defun tree-next-directory (&optional arg)
  "ポイントを先のディレクトリ行へ移動."
  (interactive "p")
  (let ((arg (prefix-numeric-value arg)))
    (forward-line)
    (dotimes (i arg)
      (while (progn
               (goto-char (next-single-char-property-change (point) 'face))
               (and (not (eobp)) (or (eolp) (invisible-p (point)))))))))
  
(defun tree-previous-directory (&optional arg)
  "ポイントを前のディレクトリ行へ移動."
  (interactive "p")
  (let ((arg (prefix-numeric-value arg)))
    (beginning-of-line)
    (dotimes (i arg)
      (while (progn
               (goto-char (previous-single-char-property-change (point) 'face))
               (and (not (bobp)) (or (eolp) (invisible-p (point)))))))))

(defun tree-occur (regexp)
  (interactive "sOccur: ")
  (tree-show-all)
  (occur regexp))

(defvar tree-view-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "tree")))
    (define-key map " "         'tree-next-line)
    (define-key map "n"         'tree-next-line)
    (define-key map [return]    'tree-next-line)
    (define-key map "p"         'tree-previous-line)
    (define-key map [backspace] 'tree-previous-line)
    (define-key map "i"         'tree-point-invert)
    (define-key map [tab]       'tree-next-visit)
    (define-key map [\S-tab]    'tree-previous-visit)
    (define-key map ">"         'tree-next-directory)
    (define-key map "<"         'tree-previous-directory)
    (define-key map "H"         'tree-hide-all)
    (define-key map "h"         'tree-point-hide)
    (define-key map "\M-H"      'tree-hide-except-point)
    (define-key map "\C-c\C-h"  'tree-hide-except-point)
    (define-key map "S"         'tree-show-all)
    (define-key map "s"         'tree-point-show)
    (define-key map "\M-S"      'tree-point-branch-show)
    (define-key map "\C-c\C-s"  'tree-point-branch-show)
    (define-key map "o"         'tree-occur)
    (define-key map "w"         'tree-copy-filename)
    (define-key map "="         'tree-at-overlays)
    (define-key map "q"         'quit-window)
    (define-key map [menu-bar tree] (cons "Tree" menu-map))
    (define-key menu-map [quit-window]   '("Quit" . quit-window))
    (define-key menu-map [dashes4] '("--"))
    (define-key menu-map [tree-buffer-to-list-file]
                '("Write list" . tree-buffer-to-list-file))
    (define-key menu-map [dashes3] '("--"))
    (define-key menu-map [tree-copy-filename]
                '("Copy Filename" . tree-copy-filename))
    (define-key menu-map [dashes2] '("--"))
    (define-key menu-map [tree-point-branch-show]
                '("Branch Show" . tree-point-branch-show))
    (define-key menu-map [tree-hide-except-point]
                '("Hide All Except Point" . tree-hide-except-point))
    (define-key menu-map [tree-show-all] '("Show All" . tree-show-all))
    (define-key menu-map [tree-hide-all] '("Hide All" . tree-hide-all))
    (define-key menu-map [tree-point-show] '("Point Show" . tree-point-show))
    (define-key menu-map [tree-point-hide] '("Point Hide" . tree-point-hide))
    (define-key menu-map [tree-point-invert]
                '("Invert" . tree-point-invert))
    (define-key menu-map [dashes1] '("--"))
    (define-key menu-map [tree-previous-visit]
                '("Previous Visit" . tree-previous-visit))
    (define-key menu-map [tree-next-visit]
                '("Next Visit" . tree-next-visit))
    (define-key menu-map [tree-previous-directory]
                '("Previous Directory" . tree-previous-directory))
    (define-key menu-map [tree-next-directory]
                '("Next Directory" . tree-next-directory))
    map))

(define-derived-mode tree-view-mode fundamental-mode "Tree-view"
  "View Tree text.
\\{tree-view-mode-map}"
  ;; `tree-next-visit' etc.
  ;; Does not work correctly with the name starting with parentheses,
  ;; so dissabled it locally.
  (and show-paren-mode (show-paren-local-mode -1))
  (add-to-invisibility-spec (cons tree-symbol t))
  (save-excursion
    (goto-char (point-min))
    ;; (overlay-recenter (point-max)) ; 変わらず
    (setq-local tree-directory-total 0)
    (setq-local tree-overlay-total 0)
    (setq-local tree-overlays-list (tree-make-overlap (tree-text2list)))
    (and tree-progress-border (> tree-directory-total tree-progress-border)
         (message "make overlay...done")))
    
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (setq-local view-read-only nil)
  (setq-local tree-directory (tree-directory-location))
  (add-hook 'change-major-mode-hook #'turn-off-tree-view nil t))

(defun turn-off-tree-view ()
  (setq-local buffer-read-only nil)
  (remove-from-invisibility-spec (cons tree-symbol t))
  (remove-overlays))

(provide 'tree-view-mode)
;; fin.
