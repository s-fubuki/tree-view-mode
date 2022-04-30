;;; tree.el -- Files tree.

;; Copyright (C) 2017, 2018, 2019, 2020, 2021, 2022 fubuki

;; Author:   fubuki@frill.org
;; Keywords: tools unix
;; Version:  @(#)$Revision: 1.51 $

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

;; Files & Directory tree print.

;; For M-x Emacs command `tree', `ftree', `dtree'.
;; For eshell command `tree'.
;; For elisp function `tree-string'.

;;; Installation:

;; (require 'tree)

;;; Code:
(require 'esh-io)
(require 'dired)
(defvar tree-view-mode 'easy-tree-view-mode)

(defconst tree-version "$Revision: 1.51 $")

(defgroup tree nil
  "Files tree."
  :group   'applications
  :version "29.0.50")

(defvar tree-opt-symbols '((?s . size) (?D . date) (?Q . quote) (?d . dir)))

(defcustom tree-options nil
  "Default option.  `tree-opt-symbols' CDR を \(date size) などと list でセットする."
  :type  '(repeat symbol)
  :group 'tree)

(defcustom tree-eshell-options tree-options
  "eshell tree Default option."
  :type  '(repeat symbol)
  :group 'tree)

(defcustom tree-ignored-directories nil
  "Ignored directories."
  :type  'regexp
  :group 'tree)

(defcustom tree-buffer-name "*Tree %s*"
  "Tree directory buffer name."
  :type  'string
  :group 'tree)

(defvar tree-date-format "%Y-%m-%d %H:%M:%S")

(defvar tree-branch-regexp
  "^\\(?1:.*[|`]-- \\)\\(?:\\[\\(?2:[-: 0-9]+?\\)\\] \\)?\\(?3:\"?.+\"?\\)")
(defvar tree-branch-term "`-- ")

(defvar tree-directory nil)

(defcustom tree-search-case-fold t
  "nil なら大文字小文字を区別."
  :type  'boolean
  :group 'tree)

(defface tree-directory
    '((t :inherit dired-directory))
  "tree view mode directory face."
  :group 'faces
  :group 'tree)

(defun tree-quote (leaf &optional path)
  "`tree-options' に従い LEAF をフォーマット文字列にして戻す.
LEAF は \(filename . attributes) のコンスセル.
PATH が non-nil ならファイル名にプリペンドしてフルパスにる."
  (let* ((file (car leaf))
         (attr (cdr leaf))
         (opts tree-options)
         (date (and (memq 'date opts) (or (file-attribute-modification-time attr) 0)))
         (size (and (memq 'size opts) (or (file-attribute-size attr) 0)))
         (file (if (car attr) (concat file "/") file))
         (file (if path (concat path file) file))
         (file (if (memq 'quote opts) (format "\"%s\"" file) file)))
    (concat
     (when (or date size)
       (format "[%s] "
               (concat
                (and size (format "%11d" size))
                (and size date " ")
                (and date (format-time-string tree-date-format date)))))
     file)))

;;;###autoload
(defun tree-directory-make-list (dir)
  "DIR を走査し Tree 構造の list data にして返す.
また `tree-options' に dir が含まれているとディレクトリのみになる.

生成される list は以下のような car がディレクトリの階層リスト構造で
dir と file は其々 \(filename . attributes) のコンスセルになる.

\(dir file file (dir file file (dir file ...)) file (dir) file file ...)"
  (let ((dir (expand-file-name dir default-directory)))
    (cons (cons dir (file-attributes dir)) (tree--directory-make-list dir))))

(defun tree--directory-make-list (dir)
  (let ((files (tree-directory-files dir))
	leaf result)
    (while files
      (setq leaf  (car files)
	    files (cdr files))
      (if (file-accessible-directory-p (expand-file-name (car leaf) dir))
          (setq result
                (cons
                 (cons
                  leaf
	          (tree--directory-make-list (expand-file-name (car leaf) dir)))
                 result))
        (push leaf result)))
    (reverse result)))

(defun tree-obj-p (elt)
  (and (consp elt) (stringp (car elt)) (listp (cdr elt))
       (string-match "[-rw]+" (nth 8 (cdr elt)))))

;;;###autoload
(defun tree-directory-insert (dir-list &optional dir-only flat)
  "'tree-directory-make-list' で作った DIR-LIST を Tree 形式でカレントバッファに挿入.
DIR-ONLY が non-nil なら Directory のみをプリント.
FLAT が non-nil なら平面構造で印字."
  (let ((dir (caar dir-list))
        (fun (if flat #'tree--directory-flat-insert #'tree--directory-insert)))
    (setq-local tree-directory dir)
    (insert dir "\n")
    (funcall fun (cdr dir-list) dir-only)))

(defun tree--directory-insert (dir-list dir-only &optional branch)
  (let ((files dir-list)
        (branch (or branch ""))
        leaf result)
    (while files
      (setq leaf  (car files)
	    files (cdr files))
      (if (and (consp leaf) (tree-obj-p (car leaf))) ; directory
          (progn
            (insert (concat branch (if files "|-- " "`-- ")) (tree-quote (car leaf)) "\n")
            (tree--directory-insert
             (cdr leaf)  dir-only (concat branch (if files "|   " "    "))))
        (if (not dir-only)
            (insert (concat branch (if files "|-- " "`-- ")) (tree-quote leaf) "\n"))))))

(defun tree--directory-flat-insert (dir-list dir-only &optional branch)
  (let ((files dir-list)
        (branch (or branch ""))
        leaf result)
    (while files
      (setq leaf  (car files)
	    files (cdr files))
      (if (and (consp leaf) (tree-obj-p (car leaf))) ; directory
          (progn
            (insert (tree-quote (car leaf) branch) "\n")
            (tree--directory-flat-insert
             (cdr leaf)  dir-only (concat branch (caar leaf) "/")))
        (if (not dir-only)
            (insert (tree-quote leaf branch) "\n"))))))

;;;###autoload
(defun tree-list-write (dir file)
  "DIR をスキャンし list 化し FILE に書き出す."
  (interactive "DScan Dir: \nFFile name: ")
  (let ((tree-options (remove 'dir tree-options)))
    (tree-write-list (tree-directory-make-list dir) file)))

;; iso8601--full-date-match などを使うと size にもマッチしてしまい誤動作する.
(defun tree-split-attributes (str)
  "tree で表示される [..] の中身を日付と時間とサイズの文字列に分割して alist で返す.
存在しない値は nil になる."
  (let (size date time)
    (dolist (a (if str (split-string str) '("")))
      (cond
       ((string-match "-" a)
        (setq date (cons 'date a)))
       ((string-match ":" a)
        (setq time (cons 'time a)))
       ((string-match " *?[[:digit:]+]" a)
        (setq size (cons 'size a)))))
    (list size date time)))

(defun tree-attribute-pack (str &optional dir)
  "擬似的な file attribute list を作る.
テキストからは得られない不明な値はすべてダミーで、今のところ tree から使うことはない."
  (let ((attr (tree-split-attributes str)))
    (list
     (if dir t) nil nil nil
     nil ; ac-time
     (encode-time
      (parse-time-string
       (mapconcat 'identity
                  (list (or (cdr (assq 'date attr)) "1970-01-01")
                        (or (cdr (assq 'time attr)) "12:00:00"))
                  " ")))
     nil ;ch-time
     (string-to-number (or (cdr (assq 'size attr)) "0"))
     (if dir "drw" "-rw")
     nil nil nil)))

;;;###autoload
(defun tree-text-make-list ()
  "Tree text をスキャンして tree 構造の list として返す.

\"(dir file file (dir file file ...) file ...)\" といった car が directory の構造のリスト.
file, dir 各エレメンツは \(name . attributes) のコンスセル.
例外的に一番外側のカッコはベースディレクトリで \(name . nil) となっている.

空のディレクトリもファイルと同じレベルになってしまうが、
tree text 側もそれは同じなので不整合はない."
  (let (dir)
    (save-excursion
      (goto-char (point-min))
      (setq dir (buffer-substring-no-properties (point) (line-end-position)))
      (forward-line)
      (cons (cons dir nil) (tree--text-make-list)))))

(defun tree-quote-strip (str)
  "クォートで括られた文字列からクォートを剥ぎ取る.
末尾にスラシュが在ればそれも取り去る."
  (let ((str
         (if (and (equal "\"" (substring str 0 1)) (equal "\"" (substring str -1)))
             (substring str 1 -1)
           str)))
    (if (equal (substring str -1) "/")
        (substring str 0 -1)
      str)))

(defun tree--text-make-list ()
  (let (branch attr name length leaf result)
    (catch 'break
      (while (re-search-forward tree-branch-regexp nil t)
        (setq branch (match-string-no-properties 1)
              attr   (match-string-no-properties 2)
              name   (tree-quote-strip (match-string-no-properties 3))
              length (length branch))
        (if (< length (tree-text-next-branch-length))
            (progn
              (forward-line)
              (setq leaf   (cons name (tree-attribute-pack attr 'dir))
                    result (cons (cons leaf (tree--text-make-list)) result)))
          (setq leaf (cons name (tree-attribute-pack attr))
                result (cons leaf result)))
        (if (string-match tree-branch-term branch)
            (throw 'break (reverse result)))))))

(defun tree-text-next-branch-length ()
  "次の行の枝の長さを返す.
それが現在の枝より長ければ現在行はディレクトリである."
  (save-excursion
    (save-match-data
      (forward-line)
      (looking-at tree-branch-regexp)
      (length (match-string-no-properties 1)))))

(defun tree-directory-files (dir)
  "`directory-files' for directory only option `dir'."
  (let ((files
	 (tree-choice (directory-files-and-attributes dir nil "[^.][^.]?\\'")))
	result)
    (if (memq 'dir tree-options)
	(dolist (a files (reverse result))
	  (if (and (cadr a)
                   (file-accessible-directory-p (expand-file-name (car a) dir)))
	      (setq result (cons a result))))
      files)))

(defun tree-choice (lst)
  "Creates and returns a LST with `tree-ignored-directories' removed from LST."
  (let ((re tree-ignored-directories)
        result)
    (if re
        (dolist (a lst (reverse result))
          (unless (string-match re (car a))
            (setq result (cons a result))))
      lst)))

;;;###autoload
(defun tree-string (dir-or-list)
  "Return tree string of DIR-OR-LIST."
  (let (dir func)
    (cond
     ((file-accessible-directory-p dir-or-list)
      (setq func #'tree-directory-make-list
            dir (expand-file-name dir-or-list default-directory)))
     (t
      (setq func #'tree-read-list
            dir (caar dir-or-list)
            dir-or-list (cdr dir-or-list))))
    (with-temp-buffer
      (tree-directory-insert (funcall func dir-or-list))
      (buffer-string))))

(defun do-eshell-tree (dir)
  (eshell-init-print-buffer)
  (eshell-buffered-print (format "%s\n" dir))
  (eshell-buffered-print (tree-string dir))
  (eshell-flush))


;;;###autoload
(defun tree-buffer-display (dir-or-list &optional prefix)
  "Directory Name  もしくは List file Name である DIR-OR-LIST を新規バッファに表示.
PREFIX が 0 ならフラット表示する." ; 引数なし prefix は予約として空けてある.
  (interactive "GFile or Dir: \np")
  (let ((buff (generate-new-buffer (format tree-buffer-name dir-or-list)))
        (flat (if (and (numberp prefix) (zerop prefix)) t))
        (func (if (file-accessible-directory-p dir-or-list)
                  #'tree-directory-make-list
                #'tree-read-list)))
    (with-current-buffer buff
      (tree-directory-insert (funcall func dir-or-list) nil flat)
      (set-buffer-modified-p nil)
      (or prefix (funcall tree-view-mode))
      (goto-char (point-min)))
    (switch-to-buffer buff)))

;;;###autoload
(defun dtree (dir &optional prefix)
  "Display of tree of directory only."
  (interactive "G\nP")
  (let ((tree-options
         (if (memq 'dir tree-options)
             tree-options
           (cons 'dir tree-options))))
    (tree-buffer-display dir prefix)))

;;;###autoload
(defun ftree (dir &optional prefix)
  "Display of \"files\" tree."
  (interactive "G\nP")
  (let ((tree-options (remove 'dir tree-options)))
    (tree-buffer-display dir prefix)))

;;;###autoload
(defun tree (dir &optional prefix)
  "Display of files tree."
  (interactive "G\nP")
  (tree-buffer-display dir prefix))

;;;###autoload
(defun eshell/tree (&rest arg)
  "Print tree."
  (let (tree-options opt dir)
    (while arg
      (if (eq (aref (car arg) 0) ?-)
          (setq opt (concat (substring (car arg) 1) opt))
        (setq dir (car arg)))
      (setq arg (cdr arg)))
    (setq tree-options
          (append
           (mapcar #'(lambda (a) (cdr (assq a tree-opt-symbols))) opt)
           tree-eshell-options))
    (when (memq nil tree-options)
      (error "usage: tree [-%s] [dir]\n"
             (apply #'string (mapcar #'car tree-opt-symbols))))
    (do-eshell-tree (or dir default-directory))))

(defun tree-write-list (lst file)
  "LST を黙って FILE に書き出す."
  (with-temp-buffer
    (let ((coding-system-for-write 'utf-8))
      (prin1 lst (current-buffer))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun tree-read-list (file)
  "リスト FILE を読み込み lisp オブジェクトとして返す."
  (let (lst)
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case err
	  (setq lst (read (current-buffer)))
	(error (error "Illegal file `%s'" file))))
    lst))

;;;###autoload
(defun tree-text-to-list-file (in-text out-file)
  "Tree Text file IN-TEXT を List data file OUT-FILE に書き出す."
  (interactive "fTree Text: \nFOut name: ")
  (with-temp-buffer
    (insert-file-contents in-text)
    (tree-write-list (tree-text-make-list) out-file)))

;;;###autoload
(defun tree-buffer-to-list-file (file)
  "Tree buffer を List data file FILE に書き出す."
  (interactive "FFile name: ")
  (tree-write-list (tree-text-make-list) file))

;;;###autoload
(defun tree-search (regexp lst &optional dir path)
  "ツリー構造の Directory LST から REGEXP を含む要素を list で返す.
マッチしなければ nil を返す.
大文字小文字は区別しないが `string-match' を使っているので \
`case-fold-search' の値で制御できる.
optional ふたつは再帰用ワーク用変数で \
DIR は現在ポイントしている要素がディレクトリなら non-nil.
PATH はポイントしている要素の path 構成を保持している."
  (let (result)
    (while lst
      (cond
       ((and (consp (car lst)) (tree-obj-p (caar lst))) ; directory        
        (setq result
              (append (tree-search regexp (car lst) 'dir (concat path (caaar lst) "/"))
                      result)))
       ((string-match regexp (caar lst))
        (setq result (cons
                      (if dir path (concat path (caar lst)))
                      result))))
      (setq lst (cdr lst) dir nil))
    (reverse result)))

(defun tree-match-highlight (regexp)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (overlay-put
       (make-overlay (match-beginning 0) (match-end 0))
       'face 'match))))

;;;###autoload
(defun dired-tree-search (regexp &optional prefix)
  "tree-search の dired インターフェイス. カーソルの File name が tree-search に渡される.
`tree-search-case-fold' が nil なら大文字小文字を区別する.
prefix 有でこの変数の値が反転する."
  (interactive "sRegexp: \nP")
  (let* ((file (dired-get-filename))
         (case-fold-search
          (if prefix (not tree-search-case-fold) tree-search-case-fold))
         (match (tree-search regexp (tree-read-list file)))
         (count (length match))
         (buff "*match list*"))
    (if (zerop count)
        (message "no match")
      (and (get-buffer buff) (kill-buffer buff))
      (with-current-buffer (get-buffer-create buff)
        (dolist (string match)
          (insert string "\n"))
        (goto-char (point-min))
        (help-mode)
        (tree-match-highlight regexp))
      (pop-to-buffer buff)
      (message "\"%s\" was matched %d." regexp count))))

;; Easy Tree view mode
(defvar easy-tree-directory-regexp "^.*[|`]-- \\(?:\\[[ :0-9-]+\\] \\)?\\(?1:.+?\\)/$")

(define-derived-mode easy-tree-view-mode fundamental-mode "Tree"
  (setq buffer-read-only t)
  (setq-local
   font-lock-defaults
   `(((,easy-tree-directory-regexp 1 'tree-directory))))
  (define-key easy-tree-view-mode-map ">" 'easy-tree-next-directory)
  (define-key easy-tree-view-mode-map "<" 'easy-tree-previous-directory)
  (define-key easy-tree-view-mode-map "q" 'quit-window))

(defun easy-tree-next-directory ()
  (interactive)
  (re-search-forward easy-tree-directory-regexp nil t)
  (goto-char (match-beginning 1)))

(defun easy-tree-previous-directory ()
  (interactive)
  (re-search-backward easy-tree-directory-regexp nil t)
  (goto-char (match-beginning 1)))

(provide 'tree)
;; fin.
