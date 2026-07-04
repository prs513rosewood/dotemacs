;; ----------- LaTeX related packages -----------

;; AUCTeX
(use-package tex-mode :straight auctex
  ;; :disabled
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq-default TeX-master nil)
  ; (turn-on-auto-fill)
  ; (bibtex-set-dialect 'biblatex)
  (setq TeX-fold-env-spec-list
        '(("[{1}:{2}]" ("frame"))))
  :gfhook
  ('TeX-mode-hook (lambda ()
                    (TeX-fold-mode 1)
                    (auto-fill-mode 0)))
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
  (TeX-engine 'luatex)
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
    "v"   #'TeX-view
    "z"   #'TeX-fold-buffer
    "Z"   #'TeX-fold-clearout-buffer
    "C"   #'TeX-command-run-all
    "h"   #'TeX-documentation-texdoc)
  (general-define-key
   :keymaps 'TeX-mode-map
   [remap compile] #'TeX-command-master))

;; Reftex for reference management
(use-package reftex
  :ghook ('LaTeX-mode-hook #'turn-on-reftex)
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

;; DocView mode
(use-package doc-view
  :custom
  (doc-view-resolution 300)
  :gfhook
  ('doc-view-minor-mode-hook #'doc-view-fit-height-to-window))
