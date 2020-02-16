;;; init.el - Emacs configuration entry point.

;;; Code:

;; Performance pre-requisites.

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

;; Initialization settings.

(require 'package)
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
(package-initialize)

;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;(let ((default-directory (expand-file-name "site-lisp/" user-emacs-directory)))
;  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(let ((local-dir (expand-file-name ".local/" user-emacs-directory)))
  (setq ad-redefinition-action 'accept
	auto-save-default nil
	create-lockfiles nil
	abbrev-file-name (concat local-dir "abbrev.el")
	auto-save-list-file-name (concat local-dir "autosave")
	backup-directory-alist (list (cons "." (concat local-dir "backup/")))
	pcache-directory (concat local-dir "pcache/")
	server-auth-dir (concat local-dir "server/")))

(setq frame-resize-pixelwise t)

(if (fboundp 'fringe-mode) (fringe-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (window-system)
  (set-frame-font "JetBrains Mono-13"))

(if (and (eq system-type 'darwin) (version< "27.0" emacs-version))
    (set-fontset-font
     "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Dracula
(unless (package-installed-p 'dracula-theme)
  (package-install 'dracula-theme))
(load-theme 'dracula t)

;; Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Smex
(unless (package-installed-p 'smex)
  (package-install 'smex))
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)
(setq which-key-add-column-padding 1
      which-key-idle-delay 0.5
      which-key-max-displays-columns nil
      which-key-min-displays-lines 5
      which-key-sort-order #'which-key-prefix-then-key-order
      which-key-sort-uppercase-first nil)
(which-key-mode)
(set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
(which-key-setup-side-window-bottom)

(require 'server)
(unless (server-running-p) (server-start))
