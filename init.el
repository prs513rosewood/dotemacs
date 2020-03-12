;; .emacs.el
;;
;; @author: Lucas Frérot
;;
;; Created with inspiration from
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://github.com/suyashbire1/emacs.d

;; This should be removed with Emascs 26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

;; Display relative line numbers (Emacs 26.1+)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative
	display-line-numbers-width-start t))

;; Recentf mode
(require 'recentf)
(recentf-mode 1)

;; Open related files in another window
(setq ff-always-in-other-window t)

;; Remove useless GUI stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Setting shell to bash
(setenv "SHELL" "/bin/bash")
(setq shell-file-name "/bin/bash")

;; Setting some environment variables
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))
(setenv "LC_ALL" "en_US.UTF-8")

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

;; Gracefully shutdown emacs server
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Consider a single space for the end of sentences
(setf sentence-end-double-space nil)

;; ----------- Package configuration -----------

;; Straight.el is used here instead of built-in package.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; ----------- Core packages -----------

;; Delight: change mode description in mode line
(use-package delight
  :commands delight
  :config (delight 'undo-tree-mode "" "undo-tree"))

;; Which-key: describes key shortcuts on-the-fly
(use-package which-key
  :config (which-key-mode 1)
  :delight)

;; General.el: keybindings definition
(use-package general
  :after which-key
  :config
  (general-override-mode 1)
  ;; Define shortcut of all modes with leader key
  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-create-definer despot-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  ;; Define ESC <-> C-g only in GUI
  (when (display-graphic-p)
    (progn
      (general-define-key
       :keymaps 'key-translation-map
       "ESC" (kbd "C-g"))))

  ;; Definition of general shortcuts and categories
  (tyrant-def
   "" nil

   "X"   #'execute-extended-command

   "f"   #'(:ignore t :which-key "files")
   "ff"  #'find-file
   "fr"  #'recentf-open-files
   "fo"  #'ff-find-other-file

   "b"   #'switch-to-buffer-other-window
   "B"   #'ibuffer-other-window
   "h"   #'help

   "c"   '(:ignore t :which-key "compile")
   "cc"  #'compile
   "cr"  #'recompile
   "ck"  #'kill-compilation
   "cq"  #'((lambda ()
	      (interactive)
	      (kill-buffer "*compilation*"))
	    :which-key "Kill compile buffer")

   "/"   #'comment-or-uncomment-region
   "TAB" #'mode-line-other-buffer
   "d"   #'dired

   "e"   #'(:ignore t :which-key "edit")
   "ev"  #'((lambda ()
	      (interactive)
	      (find-file (locate-user-emacs-file "init.el")))
	    :which-key "Edit config file")
   "sv"  #'((lambda () "Source config file"
	      (interactive)
	      (eval (locate-user-emacs-file "init.el")))
	    :which-key "Source config file")

   "wc"  #'whitespace-cleanup

   "F"   #'(:ignore t :which-key "frames")
   "Fm"  #'make-frame
   "Fk"  #'delete-frame
   "q"   #'(:ignore t :which-key "quit")
   "qs"  #'server-shutdown
   ))

;; Selectrum: simpler global completion
(use-package selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :config
  (selectrum-mode +1))

;; Prescient: better completion algo for selectrum
(use-package prescient
  :straight (prescient :host github :repo "raxod502/prescient.el"))
(use-package selectrum-prescient
  :after prescient
  :straight (prescient
	     :host github
	     :repo "raxod502/prescient.el"
	     :file ("selectrum-prescient.el"))
  :config
  (selectrum-prescient-mode))

;; Helm: global completion
(use-package helm
  :disabled
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
    "X"  'execute-extended-command
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "b" #'helm-buffers-list
    "h" #'helm-mini)
  :delight)

;; Evil mode
(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-ex-search-vim-style-regexp t "Regex in vim search")
  :config
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  :general
  (general-define-key
   :states 'normal
   "é" #'evil-ex
   ";" #'evil-ex
   "C-u" #'evil-scroll-up
   "TAB" #'indent-for-tab-command
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line))

;; ----------- QOL packages -----------

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-startub-banner 'official)
  (dashboard-center-content t))

;; Magit: git made awesome
(use-package magit
  :commands (magit-status)
  :general
  (tyrant-def
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gr" 'magit-file-delete))

;; Projectile: project management
(use-package projectile
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
    "p"  '(:ignore t :which-key "projectile")
    "pc" #'projectile-compile-project
    "pf" #'projectile-find-file
    "pb" #'projectile-switch-to-buffer-other-window
    "pB" #'projectile-ibuffer)
  :delight '(:eval (concat " " (projectile-project-name))))

;; Ripgrep: faster and project aware grep
(use-package projectile-ripgrep
  :commands projectile-ripgrep
  :general
  (general-define-key
   :states 'normal
   "<f8>" #'projectile-ripgrep))

;; Helm-projectile: helm extenstion to projectile
(use-package helm-projectile
  :disabled
  :commands helm-projectile
  :config
  (helm-projectile-on)
  :general
  (tyrant-def
   "p" 'helm-projectile))

;; Org-mode: it's org-mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general
  (despot-def
    :states 'normal
    :keymaps 'org-mode-map
    "t" 'org-todo)
  :custom
  (org-agenda-files '("~/Nextcloud/orgs"))
  (org-directory "~/Nextcloud/orgs")
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-src-fontify-natively t)
  (org-latex-pdf-process (quote ("latexmk %f")))
  (org-log-done 'time)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((python . t)))
  (with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes
			 '("talk"
			   "\\documentclass{talk}
			    [NO-DEFAULT-PACKAGES]
			    [PACKAGES]
			    [EXTRA]"
			   ("\\section{%s}" . "\\section*{%s}"))))
  (with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes
			 '("mythesis"
			   "\\documentclass{mythesis}
			    [NO-DEFAULT-PACKAGES]
			    [PACKAGES]
			    [EXTRA]"
			   ("\\chapter{%s}" . "\\chapter*{%s}")
			   ("\\section{%s}" . "\\section*{%s}")
			   ("\\subsection{%s}" . "\\subsection*{%s}")
			   ("\\subsubsection{%s}" . "\subsubsection*{%s}"))))
  :gfhook ('org-mode-hook (lambda ()
			    (visual-line-mode t)
			    (flyspell-mode t)))
)

;; Iedit: edit multiple regions simultaneously
(use-package iedit
  :commands iedit-mode
  :general
  (tyrant-def
    "ei" 'iedit-mode
    "eq" 'iedit-quit)
  (general-define-key
   :keymaps 'iedit-mode-keymap
   :states 'normal
   "n" 'iedit-next-occurrence
   "N" 'iedit-prev-occurrence
   "M-H" 'iedit-restrict-function))

;; Evil extensions
(use-package evil-magit
  :after evil magit
  :hook (magit-mode . evil-magit-init))
(use-package evil-org
  :after evil org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))
(use-package evil-snipe
  :after evil
  :config (evil-snipe-override-mode 1))

;; Company: global auto-completion
(use-package company
  :config (global-company-mode)
  :gfhook ('org-mode-hook (lambda () (company-mode -1)))
  :delight)

;; pyvenv: managing virtualenv in emacs
(use-package pyvenv
  :ghook 'python-mode-hook)

;; clang-format: cool
(use-package clang-format
  :commands clang-format-region
  :init
  (fset 'c-indent-region 'clang-format-region)
  :general
  ('normal
   "<C-tab>" 'clang-format-region)
  (tyrant-def
    :states 'normal
    :keymaps 'c-mode-base-map
    "cf" 'clang-format-buffer))

;; lsp-mode: Language Server Protocol glue
(use-package lsp-mode
  :ghook ('prog-mode-hook 'lsp-deferred)
  :custom
  (lsp-clients-clangd-executable "clangd-7")
  (lsp-keymap-prefix "s-l")
  (lsp-prefer-flymake nil))

;; lsp-ui: integration to flycheck
(use-package lsp-ui
  :ghook ('lsp-mode-hook 'lsp-ui-mode))

;; company-lsp: integration with company
(use-package company-lsp
  :ghook ('company-mode-hook (lambda () (push 'company-lsp company-backends))))

;; Flycheck: on-the-fly syntax checking
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-flake8-maximum-line-length 80)
  :custom
  (flycheck-gcc-openmp t "Activate OpenMP awareness")
  :ghook ('(lsp-ui-mode-hook tex-mode-hook))
  :config
  (global-flycheck-mode)
  :general
  (tyrant-def
   "cn" #'flycheck-next-error
   "cb" #'flycheck-previous-error)
  :delight)

;; Helm extension for flycheck
(use-package helm-flycheck
  :disabled
  :commands helm-flycheck)

;; Better C++ syntax highlighting
(use-package modern-cpp-font-lock
  :ghook ('c++-mode-hook #'modern-c++-font-lock-mode)
  :delight modern-c++-font-lock-mode)

;; Eldoc: documentation for elisp
(use-package eldoc
  :config
  (eldoc-mode t)
  :delight)

;; Irony-eldoc: irony extension for eldoc
(use-package irony-eldoc
  :after eldoc irony
  :ghook ('irony-mode-hook #'irony-eldoc))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ghook ('(prog-mode-hook LaTeX-mode-hook)))

;; Avy
(use-package avy
  :commands avy-goto-word-1
  :general (tyrant-def "SPC" #'avy-goto-word-1))

;; Flyspell
(use-package flyspell
  :ghook ('(text-mode LaTeX-mode-hook))
  :ghook ('prog-mode 'flyspell-prog-mode)
  :general
  (tyrant-def
    "s" '(:ignore t :which-key "spell")
    "sn" #'flyspell-goto-next-error))

;; Flyspell-correct: interactive correction
(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :general (tyrant-def "ss" #'flyspell-correct-wrapper))

;; Helm extension for flyspell
(use-package helm-flyspell
  :disabled
  :general
  (tyrant-def
    "s" '(:ignore t :which-key "spell")
    "ss" 'helm-flyspell-correct
    "sn" 'flyspell-goto-next-error))

;; Vi fringe
(use-package vi-tilde-fringe
  :ghook 'prog-mode-hook
  :delight)

;; Spaceline
(use-package spaceline
  :config (spaceline-spacemacs-theme)
  :gfhook ('after-load-theme-hook 'powerline-reset))

;; Base16 theme
(use-package base16-theme)

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'")

;; Fic-mode: show TODO, FIXME, etc.
(use-package fic-mode
  :ghook ('(prog-mode-hook tex-mode-hook)))

;; Magit-todos: shows TODO in git window
(use-package magit-todos
  :ghook 'magit-status)

;; Snippets
(use-package yasnippet-snippets)
(use-package yasnippet
  :config (yas-reload-all)
  :custom (yas-snippet-dirs
	   '("~/.emacs.d/snippets"
	     yasnippet-snippets-dir))
  :ghook ('prog-mode-hook #'yas-minor-mode)
  :delight)

;; SLIME: Common Lisp things
(use-package slime
  :commands slime)

;; LAMMPS Mode
(use-package lammps-mode
  :mode "\\(in\\.\\|\\.lmp\\'\\)"
  :gfhook
  ('lammps-mode-hook
   (lambda ()
     (set (make-local-variable 'compile-command)
	  (concat "mpirun -np 4 lmp -in "
		  (if buffer-file-name
		      (shell-quote-argument buffer-file-name)))))))

;; ----------- LaTeX related packages -----------

;; AUCTeX
(use-package tex-mode :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq-default TeX-master nil)
  (turn-on-auto-fill)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-PDF-mode t)
  (LaTeX-amsmath-label "eqn:")
  (LaTeX-equation-label "eqn:")
  (LaTeX-figure-label "fig:")
  (TeX-view-program-selection '((output-pdf "xdg-open")))
  (TeX-command-default "LatexMk")
  :general
  (despot-def
    :states 'normal
    :keymaps 'TeX-mode-map
    "TAB" 'LaTeX-fill-section
    "p" 'preview-section
    "i" '(:ignore t :whick-key "insert")
    "ie" 'LaTeX-environment
    "is" 'LaTeX-section
    "im" 'TeX-insert-macro)
  (tyrant-def
    :keymaps 'TeX-mode-map
    "cc" 'TeX-command-master))

;; Reftex for reference management
(use-package reftex
  :ghook ('LaTeX-mode-hook 'turn-on-reftex)
  :custom
  (reftex-plug-into-AUCTex t)
  (reftex-label-alist '(AMSTeX))
  (reftex-include-file-commands '("include" "input" "myimport"))
  :general
  (despot-def TeX-mode-map
    "r"  '(:ignore t :which-key "reftex")
    "rl" 'reftex-label
    "rc" 'reftex-citation
    "rr" 'reftex-reference
    "rt" 'reftex-toc))

;; Couple AUCTeX w/ latexmk
(use-package auctex-latexmk
  :ghook ('LaTeX-mode-hook 'auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))

;; Completion for references
(use-package company-reftex
  :defer t
  :init
  (general-add-hook 'reftex-mode-hook
		    (lambda () (add-to-list 'company-backends
					    '(company-reftex-citations
					      company-reftext-labels)))))


;; Completion for bibtex
(use-package company-bibtex
  :defer t
  :init
  (general-add-hook 'LaTeX-mode-hook
		    (lambda ()
		      (add-to-list 'company-backends
				   'company-bibtex))))

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

;; Load theme even when in terminal
(ivan/load-indexed-theme)

;; Shortcuts for theme management
(general-define-key
 "<f12>" #'ivan/cycle-theme
 "M-<f12>" #'disable-theme)

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
