;; Linux specific emacs configuration
(defun jp/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           (auto-completion
            (haskell :variables haskell-completion-backend 'intero)
            (latex))

           (c-c++ :variables
                  c-c++-default-mode-for-headers 'c++-mode
                  c-c++-enable-clang-support t
                  clang-format-style "file"
                  )

           irony-mode
           mineo-rtags

           (haskell :variables
                    haskell-completion-backend 'intero
                    haskell-enable-hindent-style "johan-tibell")

           (shell :variables
                  shell-default-height 30
                  shell-default-position 'bottom
                  shell-default-shell 'ansi-term
                  shell-default-term-shell "/usr/bin/zsh")

           rust
           yaml
           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))
  (let ((local-additional-packages
         '(
           disable-mouse
           google-c-style
           )))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))


(defun jp/dotspacemacs/config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"
  (setq org-directory "~/Documents/org/")
  (with-eval-after-load 'org
    (setq org-src-tab-acts-natively t)
    (setq org-ref-notes-directory "~/Documents/reading/"
          org-ref-bibliography-notes "~/Documents/reading/notes/index.org"
          org-ref-default-bibliography '("~/Documents/reading/notes/index.bib")
          org-ref-pdf-directory "~/Documents/reading/lib/")
    (setq helm-bibtex-bibliography "~/Documents/reading/notes/index.bib"
          helm-bibtex-library-path "~/Documents/reading/notes/"
          helm-bibtex-notes-path "~/Documents/reading/notes/index.org"
          bibtex-completion-bibliography "~/Documents/reading/notes/index.bib"
          bibtex-completion-notes-path "~/Documents/reading/notes/index.org")
    )
  )
