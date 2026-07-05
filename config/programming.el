;; Built-in LSP client
(use-package eglot
  :straight (:type built-in)
  :commands (eglot eglot-format-buffer)
  :general
  (tyrant-def "l" #'eglot)
  (despot-def
    :states 'normal
    :keymaps 'prog-mode-map
    "fb" #'eglot-format-buffer
    "d"  #'eglot-find-declaration)
  ;; this clang-format shortcut is now muscle memory
  (tyrant-def
    :states 'normal
    :keymaps 'c-mode-base-map
    "cf" #'eglot-format-buffer))

;; Built-in on-the-fly syntax checking
(use-package flymake
  :straight (:type built-in)
  :commands flymake-mode
  :general
  (tyrant-def
    "cn" #'flymake-goto-next-error
    "cb" #'flymake-goto-prev-error))

;; Treesitter: syntax highlighting
(use-package tree-sitter
  :ghook ('(c-mode-hook
            c++-mode-hook
            python-mode-hook
            emacs-lisp-mode-hook) #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

;; Code folding
(use-package kirigami
  :general
  (tyrant-def
    :states 'normal
    "z"     #'(:ignore t :which-key "fold")
    "zo"    #'kirigami-open-fold
    "zc"    #'kirigami-close-fold
    "zO"    #'kirigami-open-fold-rec
    "zC"    #'kirigami-close-fold-rec
    "z TAB" #'kirigami-toggle-fold
    "zao"   #'kirigami-open-folds
    "zac"   #'kirigami-close-folds))

;; Folding based on outline
(use-package outline
  :straight (:type built-in)
  :ghook ('(emacs-lisp-mode-hook markdown-mode-hook) #'outline-minor-mode))
;; Parsing-based folding
(use-package hideshow
  :straight (:type built-in)
  :ghook ('(c-mode-hook c++-mode-hook) #'hs-minor-mode))
(use-package savefold
  :custom
  ;;  list of symbols indicating active backends. Default: '(outline)
  (savefold-backends '(outline org hideshow))
  ;; The directory path where the serialization files are stored.
  (savefold-directory (locate-user-emacs-file "savefold"))
  ;; When using `savefold' alongside `org-mode', configure the default Org startup
  ;; visibility to ensure that the saved state applies correctly without
  ;; conflicting with internal Org visibility cycles:
  (org-startup-folded 'showeverything)
  :config
  (savefold-mode 1))

;; Fill column indicator
(use-package hl-fill-column
  :ghook
  'prog-mode-hook
  :delight)

;; Eldoc: documentation for elisp
(use-package eldoc
  :straight (:type built-in)
  :config
  (eldoc-mode t)
  :delight)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ghook
  'prog-mode-hook
  'LaTeX-mode-hook)

;; pyvenv: managing virtualenv in emacs
(use-package pyvenv
  :ghook 'python-mode-hook
  :general
  (despot-def
    :states 'normal
    :keymaps 'python-mode-map
    "a" #'pyvenv-activate))
