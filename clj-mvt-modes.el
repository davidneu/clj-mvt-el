;;; clj-mvt-modes.el --- Adds functionality to inf-clojure to support clj-mvt -*- lexical-binding: t; -*-

;; Copyright (C) 2019  David Neu

;; Author: David Neu <david@davidneu.com>

;; URL: https://github.com/davidneu/clj-mvt-modes
;; Keywords: processes, clojure
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (clojure-mode "5.6") (inf-clojure "2.1.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds functionality to the inf-clojure package to
;; support clj-mvt.

;; `clj-mvt` has two components - a minor mode called
;; `clj-mvt-repl-mode` that extends 'inf-clojure-mode' which provides
;; a Clojure REPL, and a minor mode called `clj-mvt-src-mode` that
;; extends `inf-clojure-minor-mode` which provides commands to
;; evaluate Clojure forms in clojure-mode buffers in the REPL.

;;; Code:

(require 'arc-mode)
(require 'cl-lib)
(require 'clojure-mode)
(require 'inf-clojure)

;; Usage: (clj-mvt-goto-source-location-in-jar absolute-path-to-jar file-in-jar),
;; e.g. (clj-mvt-goto-source-location-in-jar /home/me/.m2/clj-stacktrace/clj-stacktrace/0.2.8/clj-stacktrace-0.2.8.jar clj_stacktrace/utils.clj)
(defun clj-mvt-goto-source-location-in-jar (jar-filename filename-in-jar line-number)
  "Go to LINE-NUMBER of FILENAME-IN-JAR in JAR-FILENAME."
  (let ((name (format "%s:%s" jar-filename filename-in-jar)))
    (switch-to-buffer-other-window
     (or
      (find-buffer-visiting name)
      (with-current-buffer (generate-new-buffer (file-name-nondirectory filename-in-jar))
	(archive-zip-extract jar-filename filename-in-jar)
	(set-visited-file-name name)
	(setq-local default-directory (file-name-directory jar-filename))
	(setq-local buffer-read-only t)
	(set-buffer-modified-p nil)
	(set-auto-mode)
	(current-buffer))))
    (with-no-warnings
      (goto-line line-number))))

(defun clj-mvt-stacktrace-source-location ()
  "Go to the source file location corresponding to the current line in the stacktrace."
  (interactive)
  (let* ((line
	  (buffer-substring-no-properties
	   (line-beginning-position)
	   (line-end-position)))
	 (filename-line-number
	  (if (string-match "^Location: " line 0)
	      (let* ((location (replace-regexp-in-string "^Location: " "" line)))
		(split-string location ":"))
	    (let* ((file-function (split-string (substring line 1 (- (length line) 1)) "|"))
		   (location (string-trim (cl-first file-function))))
	      (split-string location ":")))))
    (cond ((= (length filename-line-number) 2)
    	   (let* ((filename (cl-first filename-line-number))
    		  (line-number (string-to-number (cl-second filename-line-number))))
    	     (switch-to-buffer-other-window
    	      (or
    	       (find-buffer-visiting filename)
    	       (find-file-noselect filename)))
	     (with-no-warnings
    	       (goto-line line-number))))
    	  ((= (length filename-line-number) 3)
    	   (let* ((jar-filename (cl-first filename-line-number))
    		  (filename (cl-second filename-line-number))
    		  (line-number (string-to-number (cl-third filename-line-number))))
    	     (clj-mvt-goto-source-location-in-jar jar-filename filename line-number)))
    	  (t
    	   (message "clj-mvt-stacktrace-source-location: invalid format")))))

(defun clj-mvt-reset ()
  "Call (clj-mvt.tools/reset (find-ns 'dev))."
  (interactive)
  (save-some-buffers)
  (inf-clojure--send-string
   (inf-clojure-proc)
   "(clj-mvt.tools/reset (find-ns 'dev))")
  (inf-clojure-switch-to-repl t))

(defun clj-mvt-testit ()
  "Call (clj-mvt.tools/testit)."
  (interactive)
  (save-some-buffers)
  (inf-clojure--send-string
   (inf-clojure-proc)
   "(clj-mvt.tools/testit)")
  (inf-clojure-switch-to-repl t))
  
(defun clj-mvt-refresh-all ()
  "Call (clojure.tools.namespace.repl/refresh-all)."
  (interactive)
  (save-some-buffers)
  (inf-clojure--send-string
   (inf-clojure-proc)
   "(clojure.tools.namespace.repl/refresh-all)")
  (inf-clojure-switch-to-repl t))

(defun clj-mvt-previous-prompt ()
  "Move the point to the previous repl prompt."
  (interactive)
  (re-search-backward "=>")
  (re-search-backward "=>")
  (end-of-line))

(defun clj-mvt-toggle-break ()
  "Call (clj-mvt.breakpoint/toggle-break)."
  (interactive)
  (inf-clojure--send-string
   (inf-clojure-proc)
   "(clj-mvt.breakpoint/toggle-break)")
  (inf-clojure-switch-to-repl t))

(defvar clj-mvt-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-p" #'clj-mvt-previous-prompt)
    (define-key map "\C-c\C-mr" #'clj-mvt-reset)
    (define-key map "\C-c\C-ma" #'clj-mvt-refresh-all)
    (define-key map "\C-c\C-md" #'clj-mvt-toggle-break)
    (define-key map "\C-c\C-mt" #'clj-mvt-testit)
    (define-key map (kbd "C-<return>") #'clj-mvt-stacktrace-source-location)
    map))

;;;###autoload
(define-minor-mode clj-mvt-repl-mode
  "A minor mode that extends `inf-clojure-mode` which provides a Clojure REPL."
  :lighter " mvt"
  :keymap clj-mvt-repl-mode-map)

(defvar clj-mvt-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-mr" #'clj-mvt-reset)
    (define-key map "\C-c\C-ma" #'clj-mvt-refresh-all)
    (define-key map "\C-c\C-md" #'clj-mvt-toggle-break)
    (define-key map "\C-c\C-mt" #'clj-mvt-testit)
    map))

;;;###autoload
(define-minor-mode clj-mvt-src-mode
  "A minor mode that extends `inf-clojure-minor-mode` which provides commands to evaluate Clojure forms in clojure-mode buffers in the REPL."
  :lighter " mvt"
  :keymap clj-mvt-src-mode-map)

;;;###autoload
(progn
  ;; both inf-clojure-mode and inf-clojure-minor-mode
  (advice-add #'inf-clojure-load-file :before (lambda (&optional _switch-to-repl _file-name) (save-some-buffers)))
  (advice-add #'inf-clojure-load-file :after (lambda (&optional _switch-to-repl _file-name) (inf-clojure-switch-to-repl t)))

  ;; both inf-clojure-mode and inf-clojure-minor-mode
  (advice-add #'inf-clojure-eval-last-sexp :before (lambda (&optional _and-go) (save-some-buffers)))
  (advice-add #'inf-clojure-eval-last-sexp :after (lambda (&optional _and-go) (inf-clojure-switch-to-repl t)))

  ;; only inf-clojure-minor-mode
  (advice-add #'inf-clojure-eval-defun :before (lambda (&optional _and-go) (save-some-buffers)))
  (advice-add #'inf-clojure-eval-defun :after (lambda (&optional _and-go) (inf-clojure-switch-to-repl t)))

  ;; only inf-clojure-minor-mode
  (advice-add #'inf-clojure-eval-buffer :before (lambda (&optional _and-go) (save-some-buffers)))
  (advice-add #'inf-clojure-eval-buffer :after (lambda (&optional _and-go) (inf-clojure-switch-to-repl t))))

(provide 'clj-mvt-modes)

;;; clj-mvt-modes.el ends here

