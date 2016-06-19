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
			  'ein
			  'linum-relative)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backup_files/")))

;; Tell emacs to always follow symbolic links
(setq-default vc-follow-symlinks t)

;; Helm
(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

;; Org-mode
(use-package org
  :config
  (setq org-agenda-files '("~/Dropbox/orgs"))
  (setq org-directory "~/Dropbox/orgs")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)))
  )

;; Which-key
(use-package which-key
  :config (which-key-mode))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

;; Company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'org-mode-hook (lambda () (company-mode -1))))

;; Evil mode
(use-package evil
  :config (global-evil-leader-mode) (evil-mode t))

;; Yasnippet
(use-package yasnippet
  :config (yas-global-mode 0))

;; Linum relative
(use-package linum-relative
  :config
  (global-linum-mode)
  (linum-relative-mode))

;; Evil extensions
(mapc (lambda (package) (use-package package))
	'(evil-surround
	  evil-magit
	  evil-org
	  evil-leader))

(global-evil-leader-mode)
(global-evil-surround-mode)

;; Evil keybindings
(define-key evil-normal-state-map "é" #'evil-ex)
(define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
(define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)

;; Leader keybindings
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "é" #'helm-M-x)
(evil-leader/set-key "b" #'helm-buffers-list)
(evil-leader/set-key "d" #'dired)
(evil-leader/set-key "ff" #'helm-find-files)
(evil-leader/set-key "fr" #'helm-recentf)
(evil-leader/set-key "TAB" #'mode-line-other-buffer)
(evil-leader/set-key "lp" #'list-packages)
(evil-leader/set-key "ev" (lambda () "Edit config file"
			    (interactive)
			    (find-file "~/.emacs.el")))
(evil-leader/set-key "sv" (lambda () "Source config file"
			    (interactive)
			    (eval "~/.emacs.el")))
;; compile bindings
(evil-leader/set-key "cc" #'compile)
(evil-leader/set-key "cr" #'recompile)
(evil-leader/set-key "ck" #'kill-compilation)
(evil-leader/set-key "cq" (lambda () "kill compile buffer"
			    (interactive)
			    (kill-buffer "*compilation*")))
(evil-leader/set-key "cn" #'flycheck-next-error)
(evil-leader/set-key "cb" #'flycheck-previous-error)
;; org bindings
(evil-leader/set-key "oa" #'org-agenda)
;; magit bindings
(evil-leader/set-key "gs" #'magit-status)
(evil-leader/set-key "gp" #'magit-pull)
(evil-leader/set-key "gP" #'magit-push)
;; ein bindings
(evil-leader/set-key "il" #'ein:notebooklist-open)
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

;; recentf mode
(require 'recentf)
(recentf-mode 1)

;; Set compile command for python scripts
(add-hook 'python-mode-hook
    (lambda ()
      (set (make-local-variable 'compile-command)
	   (concat "python " (if buffer-file-name
				 (shell-quote-argument buffer-file-name))))))

;; Remove useless GUI stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Maximize frame
(toggle-frame-maximized)

;; Safe directory variables
(add-to-list 'safe-local-variable-values
	     '(flycheck-clang-args . ("-std=c++11" "-Wno-unused-variable" "-DTAMAAS_DEBUG")))

;; Custom variables (generated automatically by emacs)
;; -----------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
