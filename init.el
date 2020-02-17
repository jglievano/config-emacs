;;; init.el - Emacs configuration entry point.

;;; Commentary:

;; Built using `org-babel' so this forwards logic to `config.el'.

;;; Code:

;; Performance.

(defconst emacs-start-time (current-time))
(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed
		    (float-time
		     (time-subtract (current-time) emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed))) t)

(setq gc-cons-threshold most-positive-fixnum ; 2^61
      gc-cons-percentage 0.6
      message-log-max 16384)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

(defvar woof-gc-cons-threshold gc-cons-threshold)

(defun woof-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun woof-restore-garbage-collection-h ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold woof-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'woof-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'woof-restore-garbage-collection-h)

(defvar woof--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist woof--file-name-handler-alist)))

;; Initialization settings.

;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;(let ((default-directory (expand-file-name "site-lisp/" user-emacs-directory)))
;  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
