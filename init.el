;; .emacs.el
;;
;; @author: Lucas Frérot
;;
;; Created with inspiration from
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://github.com/suyashbire1/emacs.d

;; ----------- Sane defalts -----------

;; Backups
; (setq my-backup-dir (concat user-emacs-directory "backup_files"))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup_files"))) ;
(setq delete-old-versions -1)		; delete excess backup versions silently

;; Version control for backups
(setq version-control t)

;; No startup screen
(setq inhibit-startup-screen t)

;; Tell emacs to always follow symbolic links
(setq-default vc-follow-symlinks t)

;; Put custom variables elsewhere
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Restore dead keys because of input method-after
(require 'iso-transl)

;; Wrap lines in text mode
(add-hook 'text-mode-hook (lambda ()
			    (visual-line-mode t)))

;; Display relative line numbers
(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative
	display-line-numbers-width-start t))

;; Recentf mode
(require 'recentf)
(recentf-mode 1)

;; Remove useless GUI stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Setting shell to bash
(setenv "SHELL" "/bin/bash")
(setq shell-file-name "/bin/bash")

;; Setting PYTHONPATH
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

;; Displaying ansi colors
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Running a hook after a theme loads
(defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

;; ----------- Package configuration -----------

(require 'package)

;; Archives
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil) ; do not load packages before startup
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

;; ----------- Core packages -----------

;; Delight: change mode description in mode line
(use-package delight :ensure t
  :commands delight
  :config (delight 'undo-tree-mode "" "undo-tree"))

;; Which-key: describes key shortcuts on-the-fly
(use-package which-key :ensure t
  :config (which-key-mode 1)
  :delight)


;; General.el: keybindings definition
(use-package general :ensure t
  :after which-key
  :config
  (general-override-mode 1)
  ;; Define shortcut of all modes with leader key
  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  ;; Define ESC <-> C-g
  (general-define-key
   :keymaps 'key-translation-map
   "ESC" (kbd "C-g"))

  ;; Definition of general shortcuts and categories
  (tyrant-def
   "" nil

   "f"  '(:ignore t :which-key "files")

   "c"  '(:ignore t :which-key "compile")
   "cc" 'compile
   "cr" 'recompile
   "ck" 'kill-compilation
   "cq" (lambda () "Kill compile buffer"
	  (interactive)
	  (kill-buffer "*compilation*"))

   "/" 'comment-or-uncomment-region
   "TAB" 'mode-line-other-buffer
   "d" 'dired

   "lp" 'list-packages

   "ev" (lambda () "Edit config file"
	  (interactive)
	  (find-file (locate-user-emacs-file "init.el")))
   "sv" (lambda () "Source config file"
	  (interactive)
	  (eval (locate-user-emacs-file "init.el")))

   "mf" 'make-frame
   "kf" 'delete-frame
   ))

;; Helm: global completion
(use-package helm :ensure t
  :after general
  :config
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action))
  :general
  (tyrant-def
   "x"  'helm-M-x
   "ff" 'helm-find-files
   "fr" 'helm-recentf
   "b" #'helm-buffers-list)
  :delight)

;; Evil mode
(use-package evil :ensure t
  :hook (after-init . evil-mode)
  :custom
  (evil-ex-search-vim-style-regexp t "Regex in vim search")
  :general
  (general-define-key
   :states 'normal
   "é" #'evil-ex
   "C-u" #'evil-scroll-up
   "TAB" #'indent-for-tab-command
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line))

;; ----------- QOL packages -----------

;; Magit: git made awesome
(use-package magit :ensure t
  :commands (magit-status)
  :general
  (tyrant-def
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status))

;; Projectile: project management
(use-package projectile :ensure t
  :commands
  (helm-projectile
   projectile-find-file
   projectile-compile)
  :init
  (put 'projectile-project-compilation-cmd 'safe-local-variable
       (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command))
					compilation-read-command))))
  :config
  (projectile-mode 1)
  :general
  (tyrant-def
    "cp" #'projectile-compile-project
    "fp" #'projectile-find-file)
  :delight '(:eval (concat " " (projectile-project-name))))

;; Helm-projectile: helm extenstion to projectile
(use-package helm-projectile :ensure t
  :commands helm-projectile
  :config
  (helm-projectile-on)
  :general
  (tyrant-def
   "p" 'helm-projectile))

;; Flycheck: on-the-fly syntax checking
(use-package flycheck :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-flake8-maximum-line-length 80)
  :custom
  (flycheck-gcc-openmp t "Activate OpenMP awareness")
  :ghook ('(prog-mode-hook tex-mode-hook))
  :config
  (global-flycheck-mode)
  :general
  (tyrant-def
   "cn" #'flycheck-next-error
   "cb" #'flycheck-previous-error)
  :delight)

;; Helm extension for flycheck
(use-package helm-flycheck :ensure t
  :commands helm-flycheck)

;; Org-mode: it's org-mode
(use-package org :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-agenda-files '("~/Nextcloud/orgs")
	org-directory "~/Nextcloud/orgs"
	org-startup-indented t
	org-startup-truncated nil
	org-src-fontify-natively t
	org-latex-pdf-process (quote ("latexmk %f")))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((python . t)))
  (with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes
					       '("talk"
						 "\\documentclass{talk}
						 [NO-DEFAULT-PACKAGES]
						 [PACKAGES]
						 [EXTRA]"
						 ("\\section{%s}" . "\\section*{%s}"))
			)
  )
  :gfhook ('org-mode-hook (lambda ()
			    (visual-line-mode t)
			    (flyspell-mode t)))
)

;; Evil extensions
(use-package evil-magit :ensure t
  :after evil magit
  :hook (magit-mode . evil-magit-init))
(use-package evil-org :ensure t
  :after evil org
  :ghook 'org-mode)
(use-package evil-surround :ensure t
  :after evil
  :config (global-evil-surround-mode))
(use-package evil-snipe :ensure t
  :after evil
  :config (evil-snipe-override-mode 1))

;; Company: global auto-completion
(use-package company :ensure t
  :config (global-company-mode)
  :gfhook ('org-mode-hook (lambda () (company-mode -1)))
  :delight)

;; Company extension for python
(use-package company-jedi :ensure t
  :after company
  :ghook ('python-mode-hook
	  (lambda () (jedi:setup) (add-to-list 'company-backends 'company-jedi)))
  :custom
  (jedi:complete-on-dot t)
  (jedi:server-args
   '("--sys-path" "/home/frerot/Documents/tamaas/build/python")))

;; clang-format: cool
(use-package clang-format :ensure t
  :commands clang-format-region
  :init
  (fset 'c-indent-region 'clang-format-region)
  :general
  ('normal
   "<C-tab>" 'clang-format-region))

;; Irony: backend for company and flycheck
(use-package irony :ensure t
  :ghook ('(c++-mode-hook c-mode-hook objc-mode-hook))
  :gfhook ('irony-mode-hook #'irony-cdb-autosetup-compile-options))

;; Company-Irony
(use-package company-irony :ensure t
  :after irony company
  :config
  (add-to-list 'company-backends 'company-irony))

;; Flycheck-Irony
(use-package flycheck-irony :ensure t
  :after irony flycheck
  :ghook ('irony-mode-hook 'flycheck-irony-setup))

;; RTags: tag system
(use-package rtags :ensure t
  :ghook ('(c-mode-hook c++-mode-hook objc-mode-hook) 'rtags-start-process-unless-running)
  :general
  (tyrant-def
    "r"  '(:ignore t :which-key "rtags")
    "rf" 'rtags-find-symbol-at-point
    "rv" 'rtags-find-virtuals-at-point
    "ri" 'rtags-imenu))

;; Better C++ syntax highlighting
(use-package modern-cpp-font-lock :ensure t
  :ghook ('c++-mode-hook #'modern-c++-font-lock-mode)
  :delight modern-c++-font-lock-mode)

;; Eldoc: documentation for elisp
(use-package eldoc :ensure t
  :config
  (eldoc-mode t)
  :delight)

;; Irony-eldoc: irony extension for eldoc
(use-package irony-eldoc :ensure t
  :after eldoc irony
  :ghook ('irony-mode-hook #'irony-eldoc))

;; Linum relative
(use-package linum-relative
  :ensure t
  :config
  (global-linum-mode)
  (linum-relative-mode)
  :gfhook ('org-mode (lambda () (linum-mode -1))))

;; Rainbow delimiters
(use-package rainbow-delimiters :ensure t
  :ghook 'prog-mode-hook)

;; Avy
(use-package avy :ensure t
  :general
  (tyrant-def
   "SPC" #'avy-goto-char))

;; Flyspell
(use-package flyspell :ensure t
  :ghook ('text-mode 'flyspell-mode)
  :ghook ('prog-mode 'flyspell-prog-mode))

;; Helm extension for flyspell
(use-package helm-flyspell :ensure t
  :general
  (tyrant-def
    "s" '(:ignore t :which-key "spell")
    "ss" 'helm-flyspell-correct
    "sn" 'flyspell-goto-next-error))

;; Vi fringe
(use-package vi-tilde-fringe :ensure t
  :ghook 'prog-mode-hook)

;; Spaceline
(use-package spaceline :ensure t
  :config (spaceline-spacemacs-theme)
  :gfhook ('after-load-theme-hook 'powerline-reset))

;; Base16 theme
(use-package base16-theme :ensure t)

;; Markdown
(use-package markdown-mode :ensure t
  :mode "\\.md\\'")

;; ----------- Themes Management -----------
;; based on: https://emacs.stackexchange.com/a/26981

(setq ivan/themes '(base16-tomorrow-night base16-tomorrow))
(setq ivan/themes-index 0)

(defun ivan/cycle-theme ()
  (interactive)
  (setq ivan/themes-index (% (1+ ivan/themes-index) (length ivan/themes)))
  (ivan/load-indexed-theme))

(defun ivan/load-indexed-theme ()
  (ivan/try-load-theme (nth ivan/themes-index ivan/themes)))

(defun ivan/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

(ivan/load-indexed-theme)
(general-define-key
 "<f12>" #'ivan/cycle-theme)

;; ----------- Programming convenience -----------

;; GDB Many windows setup
(setq gdb-many-windows 1)

;; No indentation in namespaces
(defun my-c-setup ()
   (c-set-offset 'innamespace [0]))
(general-add-hook 'c++-mode-hook 'my-c-setup)


;; Set compile command for python scripts
(general-add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "python3 " (if buffer-file-name
				       (shell-quote-argument buffer-file-name))))))

;; Set compile command for latex documents
(general-add-hook 'latex-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "latexmk -g " (if buffer-file-name
					   (shell-quote-argument buffer-file-name))))))

;; Add .cu files to c++-mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
