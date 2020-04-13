;;; php-quickhelp.el --- quickhelp at point for php
;;; Version: 0.1
;;; Author: Vincenzo Pupillo
;;; URL: https://github.com/xyzvp/php-quickhelp

;;; Commentary:
;; The project is hosted at https://github.com/xyzvp/php-quickhelp
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'thingatpt)
(require 'dom)

(defvar php-quickhelp--dir (expand-file-name (locate-user-emacs-file "php-quickhelp-manual/")))
(defvar php-quickhelp--filename "php_manual_en.json")
(defvar php-quickhelp--dest (concat php-quickhelp--dir php-quickhelp--filename))
(defvar php-quickhelp--url (concat "http://doc.php.net/downloads/json/" php-quickhelp--filename))
(defvar php-quickhelp--jq-executable (concat (executable-find "jq") " "))

(defun php-quickhelp--download-from-url (url)
  "Download a php_manual_en.json file from URL to dest path."
  (let ((dest php-quickhelp--dir))
    (unless (file-exists-p dest)
      (make-directory dest t))
    (message (format "Download php manual from %s. Please wait ..." url))
    (url-handler-mode t)
    (if (file-exists-p url)
        (progn
          (url-copy-file url php-quickhelp--dest t)
          (message (format "Downloaded php manual from %s to %s." url php-quickhelp--dest)))
      (error "Not found %s" url))))

;;;###autoload
(defun php-quickhelp--download-or-update ()
  "Download or update the manual from php.net."
  (interactive)
  (php-quickhelp--download-from-url php-quickhelp--url)
  )

;; per rimuovere i tag html da (.return)
;; fare (with-temp-buffer (insert text) (dom-texts (libxml-parse-html-region (point-min) (point))))
;; dove text è quello che si è estratto con jq
(defun php-quickhelp--function (candidate)
  "Search CANDIDATE in the php manual."
  ;; TODO: use a cache
  (let (res resl tmp)
    (setq res
          (shell-command-to-string
           (concat php-quickhelp--jq-executable " -j -M '.[\"" candidate "\"] | \"\\(.purpose)###\\(.return)###(\\(.versions))\"' " php-quickhelp--dest))
          )
    (setq resl (split-string res "###"))
    (setq tmp (cl-list* (nth 0 resl)
                        (replace-regexp-in-string "\\s-+" "\s" (string-trim
                                                                (with-temp-buffer (insert (nth 1 resl))
                                                                                  (dom-texts (libxml-parse-html-region (point-min) (point))))
                                                                ))
                         (nth 2 resl)
                         nil)
          )
    ;; a single "\n" isn't enough
    (if (string-match "^null*" res) nil (string-join tmp "\n\n"))
  ))

(defun php-quickhelp--eldoc-function (candidate)
  "Search CANDIDATE in the php manual for eldoc."
  ;;TODO: use a cache.
  (let ((res nil))
    (setq res
          (shell-command-to-string
           (concat php-quickhelp--jq-executable " -j -M '.[\"" candidate "\"] | \"\\(.prototype)\"' " php-quickhelp--dest))
          )
    (if (string-match "^null*" res) nil res)
    )
  )

;;;###autoload
(defun php-quickhelp--at-point ()
  "Show the purpose of a function at point."
  (interactive)
  (let ((candidate (symbol-at-point)))
    (when candidate
      (message (php-quickhelp--function (format "%s" candidate)))
      )
    ))

;;;###autoload
(defun php-quickhelp--eldoc-func ()
  "Php-quickhelp integration for eldoc."
  (interactive)
  (let ((candidate (symbol-at-point)))
    (when candidate
      (message (php-quickhelp--eldoc-function (format "%s" candidate)))
      )
    ))


(when (require 'company-php nil 'noerror)
;;;###autoload
(defun company-php--quickhelp (command &optional arg &rest ignored)
  "Provide quickhelp as `doc-buffer' for `company-php'."
  (interactive (list 'interactive))
  (cl-case command
    (doc-buffer
     (let ((doc (php-quickhelp--function arg)))
       (when doc
         (company-doc-buffer doc))))
    (t (apply #'company-php command arg ignored))))
)

(when (require 'company-phpactor nil 'noerror)
;;;###autoload
(defun company-phpactor--quickhelp (command &optional arg &rest ignored)
  "Provide quickhelp as `doc-buffer' for `company-phpactor'."
  (interactive (list 'interactive))
  (cl-case command
    (doc-buffer
     (let ((doc (php-quickhelp--function arg)))
       (when doc
         (company-doc-buffer doc))))
    (t (apply #'company-phpactor command arg ignored))))
)

(provide 'php-quickhelp)
;;; php-quickhelp ends here
