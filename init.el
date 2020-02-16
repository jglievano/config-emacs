(defconst emacs-start-time (current-time))
(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed
		    (float-time
		     (time-subtract (current-time) emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed))) t)

(setq gc-cons-threshold most-positive-fixnum ; 2^61
      gc-cons-percentage 0.6)

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

;; emacs config ...

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(let ((default-directory (expand-file-name "site-lisp/" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(if (and (eq system-type 'darwin) (version< "27.0" emacs-version))
    (set-fontset-font
     "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
