;;; emacs-xkcd.el --- View xkcd from Emacs
;;; Copyright 2014 Vibhav Pant <vibhavp@gmail.com>

;; This file is not apart of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'json)
(require 'url)

(defvar xkcd-alt nil)
(defvar xkcd-cur nil)

(defgroup xkcd nil
  "A xkcd reader for Emacs")

(defcustom xkcd-cache "/tmp/"
  "Directory to cache images to"
  :group 'xkcd
  :type 'directory)

(define-minor-mode xkcd-mode
  "Minor mode for viewing xkcd in Emacs"
  :lighter " xkcd"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<right>") 'xkcd-next)
	    (define-key map (kbd "<left>") 'xkcd-prev)
	    (define-key map (kbd "C-c r") 'xkcd-rand)
	    (define-key map (kbd "C-c t") 'xkcd-alt-text)
	    map))

(defun xkcd-get-json (url)
  (let ((buffer (url-retrieve-synchronously url))
        (json nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq json (buffer-substring-no-properties (+ (point) 1) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun xkcd-download (url num)
  "Download the image linked by URL. If the file arleady exists, do nothing"
  (let ((name (concat xkcd-cache (number-to-string num) ".png")))
    (if (file-exists-p name)
	nil
      (url-copy-file url name))))

(defun xkcd-get (num)
  "Get the xkcd number NUM"
  (interactive "nEnter comic number: ")
  (get-buffer-create "*xkcd*")
  (switch-to-buffer "*xkcd*")
  (if buffer-read-only
      (toggle-read-only))
  (erase-buffer)
  (if (and (boundp 'xkcd-mode) (not xkcd-mode))
      (xkcd-mode))
  (setq xkcd-cur num)
  (let ((out (if (eq num nil)
		 (xkcd-get-json "http://xkcd.com/info.0.json")
	       (xkcd-get-json (concat "http://xkcd.com/" (number-to-string num)
				      "/info.0.json"))))
	(img nil)
	(num nil)
	(title nil))
    (setq num (cdr (assoc 'num (json-read-from-string out))))
    (setq img (cdr (assoc 'img (json-read-from-string out))))
    
    ;; FIXME: This looks pretty ugly.
    (message "Downloading comic...")
    (xkcd-download img num)
    (setq title (format "%d: %s" (cdr (assoc 'num (json-read-from-string out)))
			(cdr (assoc 'safe_title (json-read-from-string out)))))
    (insert (concat title "\n"))
    (insert-image (create-image
		   (concat xkcd-cache
			   (number-to-string
			    (cdr
			     (assoc 'num (json-read-from-string out)))) ".png") 'png))
    (if (eq xkcd-cur nil)
	(setq xkcd-cur (cdr (assoc 'num (json-read-from-string out)))))
    (setq xkcd-alt (cdr (assoc 'alt (json-read-from-string out))))
    (read-only-mode)
    (message title)))

(defun xkcd-next ()
  "Get next xkcd"
  (interactive)
  (xkcd-get (+ xkcd-cur 1)))

(defun xkcd-prev ()
  "Get previous xkcd"
  (interactive)
  (xkcd-get (- xkcd-cur 1)))

(defun xkcd-rand ()
  "Show random xkcd"
  (interactive)
  (xkcd-get (random (cdr (assoc 'num (json-read-from-string
				      (get-json "http://xkcd.com/info.0.json")))))))
  
(defun xkcd-get-latest ()
  "Get the latest xkcd"
  (interactive)
  (if (and (boundp 'xkcd-mode) (not xkcd-mode))
      (xkcd-mode))
  (xkcd-get nil))

(defun xkcd-alt-text ()
  (interactive)
  (message xkcd-alt))

(provide 'emacs-xkcd)
;;; emacs-xkcd.el ends here

