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

;; Fic-mode: show todo, fixme, etc.
(use-package fic-mode
  :ghook
  'prog-mode-hook
  'tex-mode-hook)

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
