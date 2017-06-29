;; configuration only for Windows NT derived systems
(defun jp/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))
  (let ((local-additional-packages
         '(
           (auto-completion
            (latex))
           )))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))


(defun jp/dotspacemacs/config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"
  (setq org-directory "D:/insync/Documents/org/")
  (with-eval-after-load 'org
    (setq org-src-tab-acts-natively t)
    (setq org-ref-notes-directory "D:/insync/Documents/reading/"
          org-ref-bibliography-notes "D:/insync/Documents/reading/notes/index.org"
          org-ref-default-bibliography '("D:/insync/Documents/reading/notes/index.bib")
          org-ref-pdf-directory "D:/insync/Documents/reading/lib/")
    (setq helm-bibtex-bibliography "D:/insync/Documents/reading/notes/index.bib"
          helm-bibtex-library-path "D:/insync/Documents/reading/notes/"
          helm-bibtex-notes-path "D:/insync/Documents/reading/notes/index.org"
          bibtex-completion-bibliography "D:/insync/Documents/reading/notes/index.bib"
          bibtex-completion-notes-path "D:/insync/Documents/reading/notes/index.org")
    )
  )