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
(ensure-package-installed 'use-package)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backup_files/")))

;; Tell emacs to always follow symbolic links
(setq-default vc-follow-symlinks t)

;; Usefull minor modes for text
(add-hook 'text-mode-hook (lambda ()
			    (visual-line-mode t)
			    (flyspell-mode t)))
;;(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Restore dead keys because of input method-after
(require 'iso-transl)

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

;; Helm
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))
(use-package helm-flyspell
  :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; Org-mode
(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/Dropbox/orgs"))
  (setq org-directory "~/Dropbox/orgs")
  (setq org-startup-indented t)
  (setq org-startup-truncated nil)
  (setq org-src-fontify-natively t)
  (add-hook 'org-mode-hook (lambda ()
			     (visual-line-mode t)
			     (flyspell-mode t)))
  (setq org-latex-pdf-process (quote ("rubber -df %f")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)))
  )
(use-package org-bullets
  :ensure t
  :config
  ;;(setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; GNU Global
(use-package ggtags
  :ensure t
  :config (add-hook 'c-mode-common-hook (lambda () (ggtags-mode 1))))
(use-package helm-gtags
  :ensure t
  :config (add-hook 'c-mode-common-hook #'helm-gtags-mode))

;; Magit
(use-package magit
  :ensure t)

;; Which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-flake8-maximum-line-length 100))

;; Company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'org-mode-hook (lambda () (company-mode -1))))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:server-args
      '("--sys-path" "/home/frerot/Documents/tamaas/build/python")))

;; Eldoc
(use-package eldoc
  :ensure t
  :config
  (eldoc-mode t))

;; Evil mode
(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (setq evil-ex-search-vim-style-regexp t))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 0))

;; Linum relative
(use-package linum-relative
  :ensure t
  :config
  (global-linum-mode)
  (linum-relative-mode)
  (add-hook 'org-mode (lambda () (linum-mode -1))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Avy
(use-package avy
  :ensure t)

;; Evil extensions
(use-package evil-magit
  :ensure t)
(use-package evil-org
  :ensure t
  :config (add-hook #'org-mode #'evil-org-mode))
(use-package evil-leader
  :ensure t
  :config (global-evil-leader-mode))
(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode))
(use-package evil-snipe
  :ensure t
  :config (evil-snipe-override-mode 1))

;; Evil keybindings
(define-key evil-normal-state-map "é" #'evil-ex)
(define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
(define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)
;; Visual lines for wraped files
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)


;; Leader keybindings
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "é" #'helm-M-x)
(evil-leader/set-key "b" #'helm-buffers-list)
(evil-leader/set-key "d" #'dired)
(evil-leader/set-key "ff" #'helm-find-files)
(evil-leader/set-key "p" #'helm-projectile)
(evil-leader/set-key "fr" #'helm-recentf)
(evil-leader/set-key "/" #'comment-or-uncomment-region)
(evil-leader/set-key "TAB" #'mode-line-other-buffer)
(evil-leader/set-key "<SPC>" #'avy-goto-char)
(evil-leader/set-key "lp" #'list-packages)
(evil-leader/set-key "ev" (lambda () "Edit config file"
			    (interactive)
			    (find-file "~/.emacs.el")))
(evil-leader/set-key "sv" (lambda () "Source config file"
			    (interactive)
			    (eval "~/.emacs.el")))
(evil-leader/set-key "sn" #'flyspell-goto-next-error)
(evil-leader/set-key "ss" #'helm-flyspell-correct)
;; changing some helm behavior
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
;; compile bindings
(evil-leader/set-key "cc" #'compile)
(evil-leader/set-key "cr" #'recompile)
(evil-leader/set-key "ck" #'kill-compilation)
(evil-leader/set-key "cq" (lambda () "Kill compile buffer"
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

;; Vi fringe
(use-package vi-tilde-fringe
  :ensure t
  :config (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode))

;; Powerline
(use-package powerline
  :ensure t)
(use-package spaceline
  :ensure t)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-minor-modes-off)

;; Themes
(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme 'spacemacs-light t)
  (load-theme 'spacemacs-dark t))
(use-package solarized-theme
  :ensure t)
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  )
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package xresources-theme
  :ensure t)
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t)
  (enable-theme 'base16-tomorrow-night)
  )
;; (defvar my/base16-colors base16-default-dark-colors)
;; (setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
;;       evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
;;       evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
;;       evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
;;       evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
;;       evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

;; Do Re Mi
(use-package doremi
  :ensure t)
(use-package doremi-cmd
  :ensure t
  :config (setq doremi-custom-themes '(spacemacs-dark
				       spacemacs-light
				       dracula
				       base16-tomorrow-night)))

;; Ein
(use-package ein
  :ensure t)

(defun cycle-custom-themes()
  (interactive)
  (doremi-custom-themes+)
  (powerline-reset))

(global-set-key (kbd "<f12>") #'cycle-custom-themes)

;; recentf mode
(require 'recentf)
(recentf-mode 1)

;; Set compile command for python scripts
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "python " (if buffer-file-name
				       (shell-quote-argument buffer-file-name))))))

;; Set compile command for latex documents
(add-hook 'latex-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "latexmk -g " (if buffer-file-name
					   (shell-quote-argument buffer-file-name))))))

;; Remove useless GUI stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Maximize frame
(toggle-frame-maximized)

;; Add .cu files to c++-mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Close/Switch to compile buffer after compilation
;; (defun bury-compile-buffer-if-successful (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings "
;;   (if (and
;;        (string-match "compilation" (buffer-name buffer))
;;        (string-match "finished" string)
;;        (not
;;         (with-current-buffer buffer
;;           (goto-char 1)
;;           (search-forward "warning" nil t))))
;;       (run-with-timer 1 nil
;;                       (lambda (buf)
;;                         (bury-buffer buf)
;;                         (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                       buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Safe directory variables
(add-to-list 'safe-local-variable-values
	     '(flycheck-clang-args . ("-std=c++11" "-Wno-unused-variable" "-Wall"
				      "-DTAMAAS_DEBUG"
				      "-I/home/frerot/Documents/tamaas/src")))
(add-to-list 'safe-local-variable-values
	     '(flycheck-gcc-args . ("-std=c++11" "-Wno-unused-variable" "-Wall"
				    "-DTAMAAS_DEBUG"
				    "-I/home/frerot/Documents/tamaas/src")))

;; Setting PYTHONPATH
(setenv "SHELL" "zsh")
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

;; Custom variables (generated automatically by emacs)
;; -----------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (base16-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (cmake-font-lock cmake-mode helm-projectile projectile yasnippet which-key vi-tilde-fringe use-package spacemacs-theme spaceline solarized-theme rainbow-delimiters org-bullets linum-relative helm-gtags helm-flyspell ggtags flycheck evil-surround evil-snipe evil-org evil-magit evil-leader evil-avy ein dracula-theme doremi-cmd company-jedi color-theme-sanityinc-tomorrow)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values
   (quote
    ((flycheck-clang-include-path quote
				  (list "src/core" "src/model" "src/surface" "src/bem"))
     (flycheck-clang-include-path list "src/core" "src/model" "src/surface" "src/bem")
     (flycheck-clang-include-path . "src/core")
     (flycheck-clang-include-path . "/home/frerot/Documents/tamaas/src")
     (flycheck-gcc-args "-std=c++11" "-Wno-unused-variable" "-Wall" "-DTAMAAS_DEBUG" "-I/home/frerot/Documents/tamaas/src")
     (flycheck-clang-args "-std=c++11" "-Wno-unused-variable" "-Wall" "-DTAMAAS_DEBUG" "-I/home/frerot/Documents/tamaas/src"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
