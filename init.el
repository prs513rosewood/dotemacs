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

;; Garbage collection threshould to 100MB
(setq gc-cons-threshold 100000000)

;; Case insensitive completion
(setq completion-ignore-case t)

;; Backups
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup_files")))
(setq delete-old-versions -1)		; delete excess backup versions silently

;; Version control for backups
(setq version-control t)

;; No startup screen
(setq inhibit-startup-screen t)

;; Tell emacs to always follow symbolic links
(setq-default vc-follow-symlinks t)

;; Put custom variables elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)

;; Setting some environment variables
(setenv "PYTHONPATH"
  (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))
(setenv "LC_ALL" "C.UTF-8")

;; Tramp default method
(setq tramp-default-method "ssh")

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

;; Sane indent defaults
(setq-default tab-width 2
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Fill in text mode
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Main typeface
(when (find-font (font-spec :name "Roboto Mono"))
    (set-frame-font "Roboto Mono 10" nil t))

;; Visible bell
(setq visible-bell t)

;; ----------- Package configuration -----------

;; Straight.el is used here instead of built-in package.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
       user-emacs-directory))
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
;;(use-package delight
;;  :commands delight
;;  :config (delight 'undo-tree-mode "" "undo-tree"))

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
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  ;; Define ESC <-> C-g only in GUI
  (when (display-graphic-p)
    (general-define-key
     :keymaps 'key-translation-map
     "ESC" "C-g"))

  ;; Binding M-: to eval-region in visual mode
  (general-define-key
   :state 'visual
   :keymaps 'evil-visual-state-map
   "M-:"  #'eval-region)

  (general-define-key
   "C-h b" #'general-describe-keybindings
   "C-h B" #'describe-bindings
   ;; Increase/decrease text size with mouse wheel
   "<C-mouse-4>" (lambda () (interactive) (text-scale-increase 1))
   "<C-mouse-5>" (lambda () (interactive) (text-scale-decrease 1)))

  ;; Definition of general shortcuts and categories
  (tyrant-def
   ""    nil

   "X"   #'execute-extended-command

   "f"   #'(:ignore t :which-key "files")
   "ff"  #'find-file
   "fr"  #'recentf-open-files
   "fo"  #'ff-find-other-file

   "b"   #'switch-to-buffer
   "B"   #'ibuffer
   "h"   #'help

   "c"   #'(:ignore t :which-key "compile")
   "cc"  #'compile
   "cr"  #'recompile
   "ck"  #'kill-compilation
   "cq"  #'((lambda ()
              (interactive)
              (kill-buffer "*compilation*"))
            :which-key "kill-compile-buffer")

   "/"   #'comment-or-uncomment-region
   "TAB" #'mode-line-other-buffer
   "d"   #'dired

   "e"   #'(:ignore t :which-key "edit")
   "ev"  #'((lambda ()
              (interactive)
              (find-file (locate-user-emacs-file "init.el")))
            :which-key "edit-emacs-config")
   "sv"  #'((lambda () "Source config file"
              (interactive)
              (eval (locate-user-emacs-file "init.el")))
            :which-key "eval-emacs-config")

   "w"   #'(:ignore t :which-key "whitespace")
   "wc"  #'whitespace-cleanup
   "wm"  #'whitespace-mode

   "t"   #'tab-new
   "gt"  #'tab-next
   "gT"  #'tab-previous

   "F"   #'(:ignore t :which-key "frames")
   "Fm"  #'make-frame
   "Fk"  #'delete-frame
   "q"   #'(:ignore t :which-key "quit")
   "qs"  #'server-shutdown))

;; Ivy
(use-package ivy
  :disabled
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (ivy-mode 1)
  :delight)

;; Helm: global completion
(use-package helm
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
    "x"  #'helm-M-x
    "X"  #'execute-extended-command
    "ff" #'helm-find-files
    "fr" #'helm-recentf
    "b"  #'helm-buffers-list
    "h"  #'helm-mini)
  :delight)

(use-package ido
  :disabled
  :config
  (ido-mode 1)
  ;; http://ergoemacs.org/emacs/emacs_ido_setup.html
  (if (version< emacs-version "25")
      (progn (make-local-variable 'ido-separator)
             (setq ido-separator "\n"))
    (progn (make-local-variable 'ido-decorations)
           (setf (nth 2 ido-decorations) "\n")))
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t))

;; Undo package
(use-package undo-fu)

;; Evil mode
(use-package evil
  :init
  (add-hook 'after-init-hook #'evil-mode)
  :custom
  (evil-ex-search-vim-style-regexp t "Regex in vim search")
  (evil-undo-system 'undo-fu)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-shift-width 4)
  (evil-symbol-word-search t)
  :config
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  :general
  ('normal
   "é"   #'evil-ex
   "ö"   #'evil-ex
   ";"   #'evil-ex
   "C-u" #'evil-scroll-up
   "j"   #'evil-next-visual-line
   "k"   #'evil-previous-visual-line
   "gG"  #'mode-line-other-buffer
   "gb"  #'next-buffer
   "gB"  #'previous-buffer))

;; ----------- QOL packages -----------

;; Helper for regexp
(use-package re-builder
  :custom
  (reb-re-syntax 'string))

;; Nice icons
;; - remember to run all-the-icons-install-fonts
(use-package all-the-icons)
(use-package emojify)

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  ;; For emacsclient
  ;; (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-startub-banner 'official)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents . 5)(projects . 5)(agenda . 5))))

;; Magit: git made awesome
(use-package magit
  :commands magit-status
  :general
  (tyrant-def
   "g"  #'(:ignore t :which-key "git")
   "gs" #'magit-status
   "gr" #'magit-file-delete
   "gb" #'magit-blame))
(use-package magit-todos
  :after magit
  :general
  (general-define-key
   :keymaps '(magit-todos-section-map magit-todos-item-section-map)
   "jT" nil
   "jl" nil
   "j" nil)
  :config
  (magit-todos-mode)
  (custom-set-variables
   '(magit-todos-keyword-suffix "")
   '(magit-todos-keywords '("TODO" "FIXME"))))

;; Projectile: project management
(use-package projectile
  :commands (projectile-find-file projectile-compile)
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
    "pb" #'projectile-switch-to-buffer
    "pB" #'projectile-ibuffer
    "pr" #'projectile-recentf)
  (general-define-key
   :keymaps 'c-mode-base-map
   [remap compile] #'projectile-compile-project)
  :delight '(:eval (concat " " (projectile-project-name))))

;; Ripgrep: faster and project aware grep
(use-package projectile-ripgrep
  :commands projectile-ripgrep
  :general
  ('normal "<f8>" #'projectile-ripgrep))

;; Org-mode: it's org-mode
(use-package org
  :straight ; (:type built-in)
  :mode ("\\.org\\'" . org-mode)
  :general
  (tyrant-def  ; global org commands
    "o"  #'(:ignore t :which-key "org")
    "oa" #'org-agenda
    "oc" #'org-capture)
  (despot-def  ; org-mode commands
    :keymaps 'org-mode-map
    "t"  #'org-todo
    "d"  #'org-deadline
    "T"  #'org-set-tags-command
    "e"  #'org-export-dispatch
    "/"  #'org-sparse-tree
    "."  #'org-time-stamp
    "s"  #'org-schedule
    "l"  #'org-insert-link
    "w"  #'org-refile
    "|"  #'org-table-create-or-convert-from-region

    "c"  #'(:ignore t :which-key "clock")
    "ci" #'org-clock-in
    "co" #'org-clock-out
    "cs" #'org-timer-set-timer
    "c." #'org-timer-pause-or-continue
    "c_" #'org-timer-stop)

  ;; org-capture keybindings
  (general-define-key
   :keymaps 'org-capture-mode-map
   [remap evil-save-and-close]          'org-capture-finalize
   [remap evil-save-modified-and-close] 'org-capture-finalize
   [remap evil-quit]                    'org-capture-kill)
  (despot-def
    :keymaps 'org-capture-mode-map
    "c" #'org-capture-finalize
    "w" #'org-capture-refile
    "k" #'org-capture-kill)

  :config
  (add-to-list 'org-modules 'org-timer)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)))
  ;; Custom latex classes
  (general-with-eval-after-load 'ox-latex
    (progn
      (add-to-list 'org-latex-classes
                   '("talk"
                     "\\documentclass{talk}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]"
                     ("\\section{%s}" . "\\section*{%s}")))
      (add-to-list 'org-latex-classes
                   '("essay"
                     "\\documentclass{essay}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\subsubsection*{%s}")))))

  :gfhook
  ('org-mode-hook #'visual-line-mode)
  ('org-clock-in-hook
   (lambda () (unless org-timer-countdown-timer (org-timer-set-timer))))
  ('org-timer-done-hook
   (lambda () (when (executable-find "beep")
                (start-process "beep" nil "beep"))))

  :custom
  (org-directory "~/Nextcloud/orgs")
  (org-agenda-files (list org-directory))
  (org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 1)))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-src-fontify-natively t)
  (org-latex-pdf-process (quote ("latexmk %f")))
  (org-log-done 'time)
  (org-capture-templates
   `(("t" "Todo"
      entry (file+headline org-default-notes-file "Tasks")
      ,(concat "* TO" "DO %?\n  %i\n  %a"))  ; breaking keyword
     ("j" "Journal"
      entry (file+olp+datetree org-default-notes-file "Journal")
      "* %?\nEntered on %U\n  %i\n  %a")))
  (org-timer-default-timer 25)
  (org-file-apps '((auto-mode . emacs)
                   (directory . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'" . "xdg-open \"%s\""))))

;; Iedit: edit multiple regions simultaneously
(use-package iedit
  :commands iedit-mode
  :general
  (tyrant-def
    "ei" #'iedit-mode)
  (general-define-key
   :keymaps 'iedit-mode-keymap
   :states 'normal
   "n"   #'iedit-next-occurrence
   "N"   #'iedit-prev-occurrence
   "M-H" #'iedit-restrict-function))

;; Evil extensions
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-org
  :disabled
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
  :ghook 'python-mode-hook
  :general
  (despot-def
    :states 'normal
    :keymaps 'python-mode-map
    "a" #'pyvenv-activate))

;; YAML mode
(use-package yaml-mode
  :mode "\\.yml\\'")

;; Snakemake mode
(use-package snakemake-mode
  :mode "Snakefile\\'")

;; clang-format: cool
(use-package clang-format
  :commands clang-format-region
  :init
  (fset 'c-indent-region 'clang-format-region)
  :general
  (tyrant-def
    :states 'normal
    :keymaps 'c-mode-base-map
    "cf" 'clang-format-buffer))

;; lsp-mode: Language Server Protocol glue
(use-package lsp-mode
  :commands lsp
  :gfhook
  ('lsp-mode-hook #'lsp-enable-which-key-integration)
  :general
  (tyrant-def "l"  #'lsp)
  (despot-def
    :states 'normal
    :keymaps 'prog-mode-map
    "d"  #'lsp-find-definition
    "m"  #'lsp-ui-imenu
    "p"  #'lsp-ui-peek-find-references
    "fb" #'lsp-format-buffer)
  :config
  (evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)
  :custom
  (lsp-modeline-code-actions-enable t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-server-trace t)
  (lsp-log-io t)
  (lsp-clients-clangd-executable "clangd")
  (lsp-clients-clangd-args '("--log=verbose"))
  (lsp-prefer-flymake nil)
  (lsp-pylsp-configuration-sources "flake8")
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-yapf-enabled t)
  (lsp-pylsp-plugins-pydocstyle-enabled nil))


;; lsp-ui: integration to flycheck
(use-package lsp-ui
  :after lsp-mode
  :ghook ('lsp-mode-hook #'lsp-ui-mode)
  :custom
  (lsp-ui-doc-delay 5))

;; Flycheck: on-the-fly syntax checking
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-flake8-maximum-line-length fill-column)
  :custom
  (flycheck-gcc-openmp t "Activate OpenMP awareness")
  :ghook
  'lsp-ui-mode-hook
  'tex-mode-hook
  :config
  (global-flycheck-mode)
  :general
  (tyrant-def
   "cn" #'flycheck-next-error
   "cb" #'flycheck-previous-error)
  :delight)

;; Better C++ syntax highlighting
(use-package modern-cpp-font-lock
  :ghook ('c++-mode-hook #'modern-c++-font-lock-mode)
  :delight modern-c++-font-lock-mode)

;; Fill column indicator
(use-package hl-fill-column
  :ghook
  'prog-mode-hook
  :delight)

;; Eldoc: documentation for elisp
(use-package eldoc
  :config
  (eldoc-mode t)
  :delight)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ghook
  'prog-mode-hook
  'LaTeX-mode-hook)

;; Avy
(use-package avy
  :commands avy-goto-word-1
  :general (tyrant-def "SPC" #'avy-goto-word-1))

;; Flyspell
(use-package flyspell
  :ghook
  'text-mode
  'LaTeX-mode-hook
  'org-mode-hook
  :general
  (tyrant-def
    "s" '(:ignore t :which-key "spell")
    "sn" #'flyspell-goto-next-error
    "sp" #'flyspell-prog-mode))

;; Flyspell-correct: interactive correction
(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :general (tyrant-def "ss" #'flyspell-correct-wrapper))

;; Vi fringe
(use-package vi-tilde-fringe
  :ghook 'prog-mode-hook
  :delight)

;; Spaceline
(use-package spaceline
  :disabled
  :config (spaceline-spacemacs-theme)
  :gfhook ('after-load-theme-hook #'powerline-reset))

;; Base16 theme
(use-package base16-theme
  :disabled)

;; Doom themes
(use-package doom-themes)

;; Doom modeline
(use-package doom-modeline
  :ghook 'after-load-theme-hook)

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'")

;; Fic-mode: show todo, fixme, etc.
(use-package fic-mode
  :ghook
  'prog-mode-hook
  'tex-mode-hook)

;; Snippets
(use-package yasnippet-snippets :disabled)
(use-package yasnippet :disabled
  :config (yas-reload-all)
  :custom (yas-snippet-dirs
           '((expand-file-name "snippets" user-emacs-directory)
             yasnippet-snippets-dir))
  :ghook ('prog-mode-hook #'yas-minor-mode)
  :delight)

;; SLIME: Common Lisp things
(use-package slime
  :straight nil  ; Do not install via straight.el
  :commands slime)

;; LAMMPS Mode
(use-package lammps-mode
  :mode "\\(^in\\.\\|\\.lmp\\'\\)"
  :gfhook
  ('lammps-mode-hook
   (lambda ()
     (set (make-local-variable 'compile-command)
    (concat "mpirun lmp -in "
      (if buffer-file-name
          (shell-quote-argument buffer-file-name)))))))

;; Groovy Mode
(use-package groovy-mode
  :mode "Jenkinsfile")

;; ----------- LaTeX related packages -----------

;; AUCTeX
(use-package tex-mode :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq-default TeX-master nil)
  (turn-on-auto-fill)
  (bibtex-set-dialect 'biblatex)
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
  (LaTeX-biblatex-use-Biber t)
  (TeX-command-BibTex "Biber")
  :general
  (despot-def
    :states 'normal
    :keymaps 'TeX-mode-map
    "TAB" #'LaTeX-fill-section
    "p"   #'preview-section
    "i"   #'(:ignore t :whick-key "insert")
    "ie"  #'LaTeX-environment
    "is"  #'LaTeX-section
    "im"  #'TeX-insert-macro
    "v"   #'TeX-view)
  (general-define-key
   :keymaps 'TeX-mode-map
   [remap compile] #'TeX-command-master))

;; Reftex for reference management
(use-package reftex
  :ghook ('LaTeX-mode-hook 'turn-on-reftex)
  :custom
  (reftex-plug-into-AUCTex t)
  (reftex-label-alist '(AMSTeX))
  (reftex-include-file-commands '("include" "input" "myimport"))
  :general
  (despot-def
    :keymaps 'TeX-mode-map
    "r"  #'(:ignore t :which-key "reftex")
    "rl" #'reftex-label
    "rc" #'reftex-citation
    "rr" #'reftex-reference
    "rt" #'reftex-toc))

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

;; RSS feeds
(use-package elfeed
  :disabled
  :commands elfeed
  :custom
  (elfeed-db-directory "~/.config/elfeed")
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-feeds
   '("https://www.gamingonlinux.com/article_rss.php"
     "https://www.debian.org/News/news"))
  :general
  (tyrant-def
    "af" #'elfeed)
  (despot-def
    :keymaps 'elfeed-search-mode-map
    "s" #'elfeed-search-live-filter
    "R" #'elfeed-update

    "RET" #'elfeed-search-show-entry
    "S-<return>" #'elfeed-search-browse-url

    "u" #'elfeed-search-untag-all-unread
    "U" #'elfeed-search-tag-all-unread))

;; ----------- Themes Management -----------
;; based on: https://emacs.stackexchange.com/a/26981

;; (setq ivan/themes '(base16-tomorrow-night base16-tomorrow))
(setq ivan/themes '(doom-one doom-one-light))
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

;; Add .cu files to c++-mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Snippet below from https://github.com/garyo
;; https://github.com/SCons/scons/wiki/IDEIntegration#emacs-and-xemacs
;; SCons builds into a 'build' subdir, but we want to find the errors
;; in the regular source dir.  So we remove build/XXX/YYY/{dbg,final}/ from the
;; filenames.
(defun process-error-filename (filename)
  (let ((case-fold-search t))
    (setq f (replace-regexp-in-string
             "[Ss]?[Bb]uild[-_]\\(release\\|debug\\)/" "" filename))
    (cond ((file-exists-p f) f)
          (t filename))))

(setq compilation-parse-errors-filename-function 'process-error-filename)

;; Set compile command for python scripts
(general-add-hook
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'compile-command)
        (concat "python3 " (if buffer-file-name
                               (shell-quote-argument buffer-file-name))))))

;; Spellchecking with multiple dictionaries
;; https://emacs.stackexchange.com/questions/21378/spell-check-with-multiple-dictionaries
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(general-with-eval-after-load "ispell"
 (setenv "LANG" "en_US")
 (setq ispell-program-name (executable-find "hunspell")
       ispell-skip-html t
       ispell-local-dictionary-alist
       '(("en_US,fr" "[[:alpha:]]" "[^[:alpha:]]" "[']"
          nil ("-d" "en_US,fr") nil utf-8))
       ispell-dictionary "en_US,fr"
       ispell-personal-dictionary "~/.config/hunspell_personal")
 (unless (file-exists-p ispell-personal-dictionary)
   (write-region "" nil ispell-personal-dictionary nil 0)))
 ;; ispell-set-spellchecker-params has to be called
 ;; before ispell-hunspell-add-multi-dic will work
 ;; NOTE: the code below fails
 ;(ispell-set-spellchecker-params)
 ;(ispell-hunspell-add-multi-dic "en_US,fr_FR"))
