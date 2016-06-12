;; Package configuration
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Package management
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; List of packages
(ensure-package-installed 'use-package
			  'evil
			  'evil-surround
			  'evil-leader
			  'evil-magit
			  'evil-org
			  'helm
			  'org
			  'which-key
			  'flycheck
			  'company
			  'yasnippet
			  'powerline
			  'spaceline
			  'spacemacs-theme
			  'solarized-theme
			  'doremi
			  'doremi-cmd
			  'ein)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backup_files/")))

;; Helm
(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x))

;; Org-mode
(use-package org)

;; Which-key
(use-package which-key
  :config (which-key-mode))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Company
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Evil mode
(use-package evil
  :config (global-evil-leader-mode) (evil-mode t))

;; Yasnippet
(use-package yasnippet
  :config (yas-global-mode 1))

;; Evil extensions
(mapc (lambda (package) (use-package package))
	'(evil-surround
	  evil-magit
	  evil-org
	  evil-leader))

(global-evil-leader-mode)
(global-evil-surround-mode)

;; Evil keybindings
(define-key evil-normal-state-map "é" 'evil-ex)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

;; Leader keybindings
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "é" 'helm-M-x)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "cc" 'compile)
(evil-leader/set-key "cr" 'recompile)
(evil-leader/set-key "TAB" 'mode-line-other-buffer)
(evil-leader/set-key "lp" 'list-packages)
;; ein bindings
(evil-leader/set-key "il" 'ein:notebooklist-open)
;; ein save when ex :w is called
(defun ein-evil-save (old-fun &rest args)
  (if (eq major-mode 'ein:notebook-multilang-mode)
      (ein:notebook-save-notebook)
    (apply old-fun args)))
(advice-add #'evil-save :around #'ein-evil-save)

;; Powerline
(use-package powerline)
(use-package spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-minor-modes-off)

;; Themes
(use-package spacemacs-theme)
(use-package solarized-theme)

;; Do Re Mi
(use-package doremi)
(use-package doremi-cmd
  :config (setq doremi-custom-themes '(spacemacs-dark spacemacs-light)))

;; Ein
(use-package ein)

(defun cycle-custom-themes()
  (interactive)
  (doremi-custom-themes+)
  (powerline-reset))

(global-set-key (kbd "<f12>") 'cycle-custom-themes)

;; Set compile command for python scripts
(add-hook 'python-mode-hook
    (lambda ()
      (set (make-local-variable 'compile-command)
	   (concat "python " (if buffer-file-name
				 (shell-quote-argument buffer-file-name))))))

;; Remove useless GUI stuff
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Maximize frame
(toggle-frame-maximized)

;; Custom variables
;; -----------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
