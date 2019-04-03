;;; org-qiita.el --- Publish to Qiita from Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Enter your name here

;; Author: Enter your name here <name@address.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ox-qmd "1.0"))
;; Keywords: outlines
;; URL: https://github.com/ifritJP/org-qiita-el.git

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a helper package which lets you export Org posts to your
;; Qiita account.

;;; Code:

(require 'json)
(require 'ox-qmd)
(require 'rx)

;;;; Variables

(defgroup org-qiita nil
  "Org-Qiita API bridge."
  :group 'org)

(defconst org-qiita-ediff-buf-name "*org-qiita-src*")

(defconst org-qiita-post-buf-name "*org-qiita-post*")

(defconst org-qiita-exclude-edit-pattern
  (rx (or "_count"
          "body"
          ":user"
          ":updated_at")))

(defconst org-qiita-title-pattern
  "\\(^=+\n\\)\\|\\(^#[^#]\\)")

(defconst org-qiita-body-pattern
  "\\(^#.+$\\)\\|\\(^[^:\n]+\n\\)\\|\\(^.+http[s]:.+\n\\)")

(defvar org-qiita-face-post-body 'org-qiita-face-post-body)

(defvar org-qiita-token nil)

(defcustom org-qiita-export-and-post t
  "If this variable is non nil,
when org file exports, it posts the article to Qiita."
  :type 'boolean
  :group 'org-qiita)

(defcustom org-qiita-export-kill-close nil
  "If this variable is non nil,
when attrib buffer killed, it close the exported buffer."
  :type 'boolean
  :group 'org-qiita)

(defcustom org-qiita-user-id nil
  "Your login name on Qiita."
  :type 'string
  :group 'org-qiita)

(defcustom org-qiita-url "https://qiita.com/api/v2/"
  "The end point for Qiita API v2."
  :type 'string
  :group 'org-qiita)

(defface org-qiita-face-post-body
  '((t :background "dark green"))
  "Face for the post body.")

(defvar org-qiita-debug-dump-rest-respons nil)

(defvar org-qiita-items nil)

;;;; Functions

(defun org-qiita-req (method path &optional input)
  (with-temp-buffer
    (when input
      (insert input))
    (let ((opt-list
           (list (concat org-qiita-url path)
                 "-s" "-X" (format "%s" method)
                 "-H" (format "Authorization: Bearer %s" org-qiita-token)
                 (when input "-H")
                 (when input "Content-Type: application/json")
                 (when input "-d")
                 (when input "@-"))))
      (apply 'call-process-region
             (point-min) (point-max) "curl" t t nil
             (delq nil opt-list))
      (beginning-of-buffer)
      (re-search-forward "\\[\\|{")
      (setq buffer-str (buffer-substring-no-properties (1- (point)) (point-max)))
      (when org-qiita-debug-dump-rest-respons
        (message (format "%s:\n%s" (list method path) buffer-str)))
      (let ((json-object-type 'plist)
            (json-array-type 'list))
        (json-read-from-string buffer-str)))))

(defun org-qiita-get-user-info ()
  (let ((info (org-qiita-req 'GET "authenticated_user")))
    (setq org-qiita-user-id (plist-get info :id))))

(defun org-qiita-get-user-id ()
  (when (not org-qiita-user-id)
    (org-qiita-get-user-info))
  org-qiita-user-id)

(defun org-qiita-inq-items ()
  (interactive)
  (org-qiita-get-items t))

(defun org-qiita-get-items (&optional force)
  (when (or (not org-qiita-items) force)
    (setq org-qiita-items
          (org-qiita-req 'GET (format "authenticated_user/items?per_page=100"))))
  org-qiita-items)

(defun org-qiita-get-titles ()
  (mapcar (lambda (item)
            (plist-get item :title))
          (org-qiita-get-items)))

(defun org-qiita-get-item-for-title (title)
  (car (delq nil (mapcar (lambda (item)
                           (when (equal (plist-get item :title) title)
                             item))
                         (org-qiita-get-items)))))

(defun org-qiita-get-item-from-title (prompt)
  (let (title)
    (setq title (completing-read prompt (org-qiita-get-titles)))
    (org-qiita-get-item-for-title title)))

(defun org-qiita-get-title-of-this-buf ()
  "This function gets title of this buffer."
  (save-excursion
    (let (body-pos title-begin-pos title)
      (beginning-of-buffer)
      (re-search-forward org-qiita-title-pattern)
      (beginning-of-line)
      (setq body-pos (point))
      (save-excursion
	(if (re-search-backward "^=+\n" nil t)
	    ;; ==== style
	    (progn
	      (next-line)
	      (setq body-pos (point))
	      (previous-line 2)
	      )
	  ;; # style
	  (next-line)
	  (setq body-pos (point))
	  (previous-line)
	  (forward-char))
	(setq title-begin-pos (point))
	(end-of-line)
	(setq title (buffer-substring-no-properties title-begin-pos (point))))
      (goto-char body-pos)
      (when (string-match "^[[:space:]]*\\([^[:space:]]*\\)[[:space:]]*$" title)
	(setq title (replace-match "\\1" nil nil title)))      
      (list title body-pos))))

(defun org-qiita-get-item-for-this-buf (select-title)
  "This function gets item for this buffer."
  (let (title item)
    (setq title (car (org-qiita-get-title-of-this-buf)))
    (setq item (org-qiita-get-item-for-title title))
    (when (and (not item) select-title)
      (setq item (org-qiita-get-item-from-title "not found title. select!: ")))
    item))

(defun org-qiita-insert-newline-at-eof ()
  "ファイル終端が改行されていない場合は改行する"
  (save-excursion
    (end-of-buffer)
    (when (not (equal
		(buffer-substring-no-properties (1- (point-max)) (point-max)) "\n"))
      (insert "\n"))))

(defun org-qiita-ediff ()
  "This function executes ediff."
  (interactive)
  (let ((draft-buf (current-buffer))
        title item)
    ;; 終端が改行されていないと diff がエラーするので、改行を入れる
    (org-qiita-insert-newline-at-eof)

    (setq item (org-qiita-get-item-for-this-buf t))
    (save-current-buffer
      (switch-to-buffer-other-window (get-buffer-create org-qiita-ediff-buf-name))
      (erase-buffer)
      (insert (plist-get item :body))
      (ediff-buffers (current-buffer) draft-buf))))

;;;###autoload
(defun org-qiita-post ()
  "This function posts an article of this buffer."
  (interactive)
  (org-qiita-insert-newline-at-eof)
  (let (item info overlay)
    (setq item (org-qiita-get-item-for-this-buf nil))
    (when (not item)
      (if (yes-or-no-p "Is this new article? ")
          (setq item '(:title ""))
        (setq item (org-qiita-get-item-from-title "select org title: "))))
    (setq item (copy-sequence item))
    (save-excursion
      (setq info (org-qiita-get-title-of-this-buf))

      ;; body 開始位置を見つける。
      ;; Author: や email: を飛す
      (goto-char (cadr info))
      (re-search-forward org-qiita-body-pattern)
      (when (not (re-search-backward "^#.+$" (cadr info) t))
        (previous-line))
      (plist-put item :body
                 (buffer-substring-no-properties (point) (point-max)))
      (plist-put item :title (car info))

      ;; body 部分を hilight する
      (setq overlay (make-overlay (point) (point-max)))
      (overlay-put overlay 'face org-qiita-face-post-body)
      (org-qiita-post-edit-attrib item overlay (current-buffer)))))

(defun org-qiita-post-edit-attrib (item body-overlay src-buf)
  "This function opens a buffer to edit attributes for an artible."
  (switch-to-buffer-other-window (get-buffer-create org-qiita-post-buf-name))
  (erase-buffer)
  ;; 必要な情報を補完
  (let (key-info)
    (setq key-info '((:coediting :json-false "t ==> true, :json-false ==> false")
                     (:gist :json-false "t ==> true, :json-false ==> false")
                     (:group_url_name nil)
                     (:private :json-false "t ==> true, :json-false ==> false")
                     (:tags ((:name "TAG")) " <--- input tag names. ex: ( \"abc\" \"xyz\" )")
                     (:title "タイトル")
                     (:tweet :json-false "t ==> true, :json-false ==> false")))
    (dolist (info key-info)
      (when (not (plist-get item (car info)))
        (plist-put item (car info) (cadr info))))
    (let (key-list key)
      (setq key-list
            (delq nil (mapcar (lambda (val)
                                (if key
                                    (setq key nil)
                                  (setq key val)))
                              item)))
      (setq key-list (sort key-list (lambda (val1 val2)
                                      (string< (symbol-name val1) (symbol-name val2)))))
      (setq key-list (append '(:title :tags) (delq :tags (delq :title key-list))))
      (insert ";;; post --> C-c C-c,   cancel --> kill buffer\n")
      (insert "'(\n")
      (dolist (key key-list)
        (when (not (string-match org-qiita-exclude-edit-pattern (symbol-name key)))
          (cond
           ((equal key :tags)
            (insert ":tags (")
            (if (plist-get item key)
                (dolist (tag (plist-get item key))
                  (insert (format "\"%s\" " (or (plist-get tag :name) ""))))
              (insert "\"TAG\""))
            (insert ") "))
           (t
            (let ((val (plist-get item key))
                  formed-val)
              (if (stringp val)
                  (setq formed-val (format "\"%s\"" val))
                (setq formed-val (format "%s" val)))
              (insert (format "%s %s" key formed-val)))))
          (when (nth 2 (assoc key key-info))
            (insert (format "  ;; %s" (nth 2 (assoc key key-info)))))
          (insert "\n"))))
    (insert ")\n")
    (beginning-of-buffer)
    (lisp-mode)
    (local-set-key (kbd "C-c C-c") 'org-qiita-post-with-attrib)
    (set (make-local-variable 'org-qiita-body-overlay) body-overlay)
    (set (make-local-variable 'org-qiita-post-item) item)
    (set (make-local-variable 'org-qiita-src-buf) src-buf)

    ;; バッファを消したときに overlay を削除する
    (if (fboundp 'make-local-hook)
        (make-local-hook 'kill-buffer-hook))
    (add-hook 'kill-buffer-hook 'org-qiita-kill-attrib-bug nil t)))

(defun org-qiita-kill-attrib-bug ()
  (delete-overlay org-qiita-body-overlay)
  (if org-qiita-export-kill-close
      (when (buffer-live-p org-qiita-src-buf)
	(kill-buffer org-qiita-src-buf)
	(delete-window))
    (let ((window (get-buffer-window org-qiita-src-buf)))
      (when window
	(select-window window)))))

(defun org-qiita-post-with-attrib ()
  "This function posts an article with specified attributes."
  (interactive)
  (when (y-or-n-p "post?")
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (id (plist-get org-qiita-post-item :id))
          tags item result posted-item)
      (with-temp-buffer
        (insert (format "(setq item %s)" text))
        (eval-current-buffer))
      (plist-put item :body (plist-get org-qiita-post-item :body))
      (dolist (tag (plist-get item :tags))
        (setq tags (cons (json-add-to-object (json-new-object) ":name" tag)
                         tags)))
      (plist-put item :tags tags)
      (with-temp-buffer
        (let ((method 'POST)
              (path "items"))
          (when id
            (setq method 'PATCH)
            (setq path (format "items/%s" id)))
          (setq result (org-qiita-req method path
                                      (json-encode-plist item)))))
      (org-qiita-get-items t)
      (setq posted-item (org-qiita-get-item-for-title (plist-get item :title)))
      (when (not (equal (plist-get posted-item :body)
                        (plist-get item :body)))
        (error (format "%s" result))))
    (kill-buffer)
    (message "ok")))

(defadvice org-qmd-export-as-markdown
    (after org-qiita-org-qmd-export-as-markdown activate)
  (when org-qiita-export-and-post
    (org-qiita-post)))

(cond ((fboundp 'org-qmd--unfill-paragraph)
       ;; 古いバージョンと今のバージョンで org-qmd のシンボルが違うので,
       ;; 両方で動くように対応。
       (org-export-define-derived-backend 'qiita 'md
         :filters-alist '((:filter-paragraph . org-qmd--unfill-paragraph))
         :menu-entry
         '(?Q "Export to Qiita Markdown"
              ((?Q "Upload with temporary buffer"
                   (lambda (a s v b) (org-qiita-export-as-markdown a s v)))))
         :translate-alist '((headline . org-qmd--headline)
                            (inner-template . org-qmd--inner-template)
                            (template . org-qiita-template)
                            (keyword . org--qmd-keyword)
                            (strike-through . org-qmd-strike-through)
                            (src-block . org-qmd--src-block))))
      (t
       (org-export-define-derived-backend 'qiita 'md
         :filters-alist '((:filter-paragraph . org-qmd-unfill-paragraph))
         :menu-entry
         '(?Q "Export to Qiita Markdown"
              ((?Q "Upload with temporary buffer"
                   (lambda (a s v b) (org-qiita-export-as-markdown a s v)))))
         :translate-alist '((headline . org-qmd-headline)
                            (inner-template . org-qmd-inner-template)
                            (template . org-qiita-template)
                            (keyword . org-qmd-keyword)
                            (strike-through . org-qmd-strike-through)
                            (src-block . org-qmd-src-block)
                            (table-cell . org-qmd-table-cell)
                            (table-row . org-qmd-table-row)
                            (table . org-qmd-table)))))

(defun org-qiita-export-as-markdown (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'qiita "*Org Qiita Upload*"
    async subtreep visible-only nil nil (lambda () (text-mode)))
  (when org-qiita-export-and-post
    (org-qiita-post)))

(defun org-qiita-template (contents info)
  "Return complete document string after conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Document title.
   (let ((title (plist-get info :title)))
     (when title
       (format "%s\n=======\n" (org-export-data title info))))
   contents))

(provide 'org-qiita)
;;; org-qiita.el ends here
