;;; php-quickhelp.el --- Quickhelp at point for php -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Vincenzo Pupillo
;; Version: 0.3.3
;; Author: Vincenzo Pupillo
;; URL: https://github.com/vpxyz/php-quickhelp
;; Package-Requires: ((emacs "25.1"))
;;; Commentary:
;; The project is hosted at https://github.com/vpxyz/php-quickhelp
;; The latest version, and all the relevant information can be found there.
;;
;; First of all you must install jq (https://stedolan.github.io/jq/).
;; Second run php-quickhelp-download-or-update.
;;
;; php-quickhelp can be used with or without company-php and company-quickhelp.
;; When used with company-php and company-quickhelp, it works like a wrapper for company-php or company-phpactor.
;; As an example, for company-phpactor,  you can do:
;; (add-hook 'php-mode-hook (lambda ()
;;     ;; .... other configs
;;     (require 'php-quickhelp)
;;     (set (make-local-variable 'company-backends)
;;     '(php-quickhelp-company-phpactor company-web-html company-dabbrev-code company-files))
;; (company-mode)))
;;
;; It's possible to use php-quickhelp as eldoc backend
;; (setq eldoc-documentation-function
;;       'php-quickhelp-eldoc-func)
;;
;; The function php-quickhelp-at-point can be used to show in the minibuffer the documentation.
;; For detail see: https://github.com/vpxyz/php-quickhelp


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
(require 'subr-x)
(require 'url-http)
(require 'shr)

(declare-function company-doc-buffer "ext:company.el")
(declare-function company-php "ext:company-php.el")
(declare-function company-phpactor "ext:company-phpactor.el")

(defvar php-quickhelp--dir (expand-file-name (locate-user-emacs-file "php-quickhelp-manual/")))
(defvar php-quickhelp--filename "php_manual_en.json")
(defvar php-quickhelp--dest (concat php-quickhelp--dir php-quickhelp--filename))
(defvar php-quickhelp--url (concat "http://doc.php.net/downloads/json/" php-quickhelp--filename)) ;; https isn't available on doc.php.net
(defvar php-quickhelp--jq-executable (concat (executable-find "jq") " "))
(defvar php-quickhelp--eldoc-cache (make-hash-table :test 'equal))
(defvar php-quickhelp--company-cache (make-hash-table :test 'equal))

(defun php-quickhelp--download-from-url (url)
  "Download a php_manual_en.json file from URL to dest path."
  (let ((dest php-quickhelp--dir))
    (unless (file-exists-p dest)
      (make-directory dest t))
    (message "Download php manual from %s. Please wait ..." url)
    (url-handler-mode t)
    (if (url-http-file-exists-p url)
        (progn
          (url-copy-file url php-quickhelp--dest t)
          (message "Downloaded php manual from %s to %s." url php-quickhelp--dest))
      (error "Not found %s" url))))

(defun php-quickhelp-download-or-update ()
  "Download or update the manual from php.net."
  (interactive)
  (php-quickhelp--download-from-url php-quickhelp--url))

(defun php-quickhelp--html2fontify-string (doc)
  "Convert html DOC to fontified string."
  (with-temp-buffer (insert doc)
                    (setq-local shr-use-fonts nil)
                    (setq-local shr-cookie-policy nil)
                    (setq-local shr-use-colors nil)
                    (setq-local shr-discard-aria-hidden t)
                    (setq-local shr-inhibit-images t)
                    (setq-local indent-tabs-mode nil)
                    (shr-render-region (point-min) (point))
                    ;; remove last '\n'
                    (let ((s (buffer-string)))
                    (if (string-match "[ \t\n\r]+\\'" s)
                        (replace-match "" t t s)
                      s))
                    ))

(defun php-quickhelp--function (candidate)
  "Search CANDIDATE in the php manual."
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (or (gethash candidate php-quickhelp--company-cache)
      (let (result tmp-strings)
        (setq result
              (shell-command-to-string
               (concat php-quickhelp--jq-executable " -j -M '.[\"" candidate "\"] | \"\\(.purpose)###\\(.return)###(\\(.versions))\"' " php-quickhelp--dest)))
        (unless (string-match "^null*" result)
          (setq tmp-strings (split-string result "###"))
          (setcar (nthcdr 1 tmp-strings) (php-quickhelp--html2fontify-string (nth 1 tmp-strings)))
          ;; sometimes "return" content is empty, better remove it
          (when (string= (string-trim (nth 1 tmp-strings)) "")
            (setq tmp-strings (remove (nth 1 tmp-strings) tmp-strings)) nil)
          ;; a single "\n" isn't enough
          (puthash candidate (string-join tmp-strings "\n\n") php-quickhelp--company-cache)))))

(defun php-quickhelp--eldoc-function (candidate)
  "Search CANDIDATE in the php manual for eldoc."
  (or (gethash candidate php-quickhelp--eldoc-cache)
      (let (result tmp-string arguments pos)
        (setq result
              (shell-command-to-string
               (concat php-quickhelp--jq-executable " -j -M '.[\"" candidate "\"] | \"\\(.prototype)\"' " php-quickhelp--dest)))
        (unless (string-match "^null*" result)
          (setq tmp-string (split-string result " "))
          (when tmp-string
            (cl-dolist (arg tmp-string)
              (setq arguments
                    (concat arguments
                            (if (setq pos (string-match "\(" arg))
                                (concat (propertize (substring arg 0 pos) 'face 'font-lock-function-name-face) (substring arg pos))
                              (if (string-match "\\$" arg)
                                  (propertize arg 'face '(:weight bold))
                                arg))
                            " "))))
          (puthash candidate arguments php-quickhelp--eldoc-cache)))))

(defun php-quickhelp--from-candidate2jq (candidate)
  "Escape CANDIDATE properly for jq."
  (let (tmp-string)
    ;; a static php function call needs more work
    (if (not (eq (get-text-property 0 'face candidate) 'php-static-method-call))
        (setq tmp-string (substring-no-properties candidate))
      ;; thing-at-point-looking-at requires a dirty trick in order to handle long static function call
      (when (thing-at-point-looking-at "\\sw*\\\\*\\sw*\\\\*\\sw*\\\\*\\sw+::\\sw+")
        (setq tmp-string (buffer-substring (match-beginning 0) (match-end 0)))))
    (replace-regexp-in-string (regexp-quote "\\") "\\\\" tmp-string t t)))

(defun php-quickhelp-at-point ()
  "Show the purpose of a function at point."
  (interactive)
  (let ((candidate (thing-at-point 'symbol)))
    (when candidate
      (message "%s" (php-quickhelp--function (php-quickhelp--from-candidate2jq candidate))))))

(defun php-quickhelp-eldoc-func ()
  "Php-quickhelp integration for eldoc."
  (let ((candidate (thing-at-point 'symbol)))
    (when candidate
      (message (php-quickhelp--eldoc-function (php-quickhelp--from-candidate2jq candidate))))))

(when (require 'company-php nil 'noerror)
  (defun php-quickhelp-company-php (command &optional arg &rest ignored)
    "Provide quickhelp as `doc-buffer' for `company-php'."
    (cl-case command
      (doc-buffer
       (let ((doc (php-quickhelp--function arg)))
         (when doc
           (company-doc-buffer doc))))
      (t (apply #'company-php command arg ignored)))))

(when (require 'company-phpactor nil 'noerror)
  (defun php-quickhelp-company-phpactor (command &optional arg &rest ignored)
    "Provide quickhelp as `doc-buffer' for `company-phpactor'."
    (cl-case command
      (doc-buffer
       (let ((doc (php-quickhelp--function arg)))
         (when doc
           (company-doc-buffer doc))))
      (t (apply #'company-phpactor command arg ignored)))))

(provide 'php-quickhelp)
;;; php-quickhelp.el ends here
