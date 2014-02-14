;;; xkcd.el --- View xkcd from Emacs

;;; Copyright (C) 2014 Vibhav Pant <vibhavp@gmail.com>

;; Url: https://github.com/vibhavp/emacs-xkcd
;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Requires: ((json "1.3"))
;; Keywords: xkcd webcomic

;;; Commentary:

;; emacs-xkcd uses the JSON interface provided by xkcd (http://xkcd.com)
;; to fetch comics.
;; Comics can be viewed offline as they are stored by default in
;; ~/.emacs.d/xkcd/
;; For more information, visit https://github.com/vibhavp/emacs-xkcd
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'json)
(require 'url)

;;;###autoload
(define-derived-mode xkcd-mode special-mode "xkcd"
  "Major mode for viewing xkcd (http://xkcd.com/) comics."
  :group 'xkcd)

(define-key xkcd-mode-map (kbd "<right>") 'xkcd-next)
(define-key xkcd-mode-map (kbd "<left>") 'xkcd-prev)
(define-key xkcd-mode-map (kbd "r") 'xkcd-rand)
(define-key xkcd-mode-map (kbd "t") 'xkcd-alt-text)
(define-key xkcd-mode-map (kbd "q") 'xkcd-kill-buffer)

(defvar xkcd-alt nil)
(defvar xkcd-cur nil)
(defvar xkcd-latest 0)

(defgroup xkcd nil
  "A xkcd reader for Emacs"
  :group 'multimedia)

(defcustom xkcd-cache-dir "~/.emacs.d/xkcd/"
  "Directory to cache images and json files to."
  :group 'xkcd
  :type 'directory)

(defcustom xkcd-cache-latest (concat xkcd-cache-dir "latest")
  "File to store the latest cached xkcd number in.
Should preferably be located in `xkcd-cache-dir'."
  :group 'xkcd
  :type 'file)

(defun xkcd-get-json (url &optional num)
  "Fetch the Json coming from URL.
If the file NUM.json exists, use it instead.
If NUM is 0, always download from URL.
The return value is a string."
  (let* ((file (format "%s%d.json" xkcd-cache-dir num))
         (cached (and (file-exists-p file) (not (eq num 0)))))
    (with-current-buffer (if cached
			     (find-file file)
			   (url-retrieve-synchronously url))
      (goto-char (point-min))
      (unless cached (re-search-forward "^$"))
      (prog1
	  (buffer-substring-no-properties (point) (point-max))
	(kill-buffer (current-buffer))))))

(defun xkcd-get-image-type (url)
  "Return a symbol (`png', `jpg' or `gif') corresponding to the last characters of URL."
  (let ((substr (substring url (- (length url) 3))))
   (cond
    ((string= substr "png")
     'png)
    ((string= substr "jpg")
     'jpg)
    (t 'gif))))

(defun xkcd-download (url num)
  "Download the image linked by URL to NUM.  If NUM arleady exists, do nothing."
  ;;check if the cache directory exists
  (unless (file-exists-p xkcd-cache-dir)
    (make-directory xkcd-cache-dir))
  (let ((name (format "%s%s.%s" xkcd-cache-dir (number-to-string num)
		      (substring url (- (length url) 3)))))
    (if (file-exists-p name)
	name
      (url-copy-file url name))
    name))

(defun xkcd-cache-json (num json-string)
  "Save xkcd NUM's JSON-STRING to cache directory and write xkcd-latest to a file."
  (let ((name (format "%s%d.json" xkcd-cache-dir num)))
    (if (> num xkcd-latest)
	(with-current-buffer (find-file xkcd-cache-latest)
	  (setq xkcd-latest num)
	  (erase-buffer)
	  (insert (number-to-string num))
	  (save-buffer)
	  (kill-buffer (current-buffer))))
    (unless (file-exists-p name)
      (with-current-buffer (find-file name)
	(insert json-string)
	(save-buffer)
	(kill-buffer (current-buffer))))))

(defun xkcd-insert-image (file num)
  "Insert image described by FILE and NUM in buffer with the title-text.
If the image is a gif, animate it."
  (let ((image (create-image (format "%s%d.%s" xkcd-cache-dir
				     num
				     (substring file (- (length file) 3)))
			     (xkcd-get-image-type file)))
	(start (point)))
    (insert-image image)
    (if (or
         (and (fboundp 'image-multi-frame-p)
              (image-multi-frame-p image))
         (and (fboundp 'image-animated-p)
              (image-animated-p image)))
	(image-animate image 0 t))
    (add-text-properties start (point) '(help-echo xkcd-alt))))

;;;###autoload
(defun xkcd-get (num)
  "Get the xkcd number NUM."
  (interactive "nEnter comic number: ")
  (xkcd-update-latest)
  (get-buffer-create "*xkcd*")
  (switch-to-buffer "*xkcd*")
  (xkcd-mode)
  (let (buffer-read-only)
    (erase-buffer)
    (setq xkcd-cur num)
    (let* ((url (if (eq num 0)
                    "http://xkcd.com/info.0.json"
                  (format "http://xkcd.com/%d/info.0.json" num)))
           (out (xkcd-get-json url num))
           (json-assoc (json-read-from-string out))
           (img (cdr (assoc 'img json-assoc)))
           (num (cdr (assoc 'num json-assoc)))
           (safe-title (cdr (assoc 'safe_title json-assoc)))
           title file)
      (message "Getting comic...")
      (setq file (xkcd-download img num))
      (setq title (format "%d: %s" num safe-title))
      (insert title "\n")
      (xkcd-insert-image file num)
      (if (eq xkcd-cur 0)
          (setq xkcd-cur num))
      (xkcd-cache-json num out)
      (setq xkcd-alt (cdr (assoc 'alt json-assoc)))
      (message title))))

(defun xkcd-next ()
  "Get next xkcd."
  (interactive)
  (xkcd-get (+ xkcd-cur 1)))

(defun xkcd-prev ()
  "Get previous xkcd."
  (interactive)
  (xkcd-get (- xkcd-cur 1)))

(defun xkcd-rand ()
  "Show random xkcd."
  (interactive)
  (let* ((url "http://xkcd.com/info.0.json")
         (last (cdr (assoc 'num (json-read-from-string
                                 (xkcd-get-json url 0))))))
    (xkcd-get (random last))))

(defun get-xkcd-from-url (url)
  "Load xkcd pointed to by URL"
  (let* ((string (substring url (string-match "[0-9]+" url)))
	 (number (substring string 0 (string-match "/" string))))
    (xkcd-get (string-to-number number))))

(setq browse-url-browser-function (lambda (url etc) (if 
						    (string-match
						     "xkcd.com/[0-9]+"
						     "http://xkcd.com/123/")
						    (get-xkcd-from-url url)
						    'browse-url-default-browser)))

(defun xkcd-get-latest ()
  "Get the latest xkcd."
  (interactive)
  (xkcd-get 0))

(defalias 'xkcd 'xkcd-get-latest)

(defun xkcd-get-latest-cached ()
  "Get the latest cached xkcd."
  (interactive)
  (xkcd-update-latest)
  (xkcd-get xkcd-latest))

(defun xkcd-alt-text ()
  "View the alt text in the buffer."
  (interactive)
  (message xkcd-alt))

(defun xkcd-kill-buffer ()
  "Kill the xkcd buffer."
  (interactive)
  (kill-buffer "*xkcd*"))

(defun xkcd-update-latest ()
  "Update `xkcd-latest' to point to the last cached comic."
  (let ((file xkcd-cache-latest))
    (with-current-buffer (find-file file)
      (setq xkcd-latest (string-to-number
			 (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer (current-buffer)))))

(provide 'xkcd)
;;; xkcd.el ends here
