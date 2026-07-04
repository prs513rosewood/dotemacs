;; YAML mode
(use-package yaml-mode
  :mode "\\.yml\\'")

;; Rust mode
(use-package rust-mode
  :mode "\\.rs\\'")

;; Jinja mode
(use-package jinja2-mode
  :mode "\\.jinja2\\'")

;; Typst mode
(use-package typst-ts-mode
  :mode "\\.typ\\'")

;; Snakemake mode
(use-package snakemake-mode
  :mode "Snakefile\\'")

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

;; Meson mode
(use-package meson-mode
  :mode "meson.build")

;; Groovy Mode
(use-package groovy-mode
  :mode "Jenkinsfile")

;; Markdown
(use-package markdown-mode
  :mode "\\(\\.md\\|\\.qmd\\'\\)")
