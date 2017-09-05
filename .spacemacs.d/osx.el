;; configuration only for macOS systems
(defun jp/dotspacemacs/os-layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           (auto-completion
            (latex))
           latex

           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))
  (let ((local-additional-packages
         '(

           )))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))


(defun jp/dotspacemacs/os-config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"
  ;; we don't find the correct tex distribution for some reason
  ;; (add-to-list 'exec-path "/usr/local/texlive/2017/bin/x86_64-darwin")
  ;; (add-to-list 'exec-path "/Library/TeX/texbin/")

  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  (add-to-list 'exec-path "/Library/TeX/texbin/")

  ;; OS X ls doesn't support --dired flag
  (setq dired-use-ls-dired nil)

  (setq-default mac-right-option-modifier nil)
  (setq magit-repository-directories '("~/src/"))

  ;; (setq org-directory "~/Documents/org/")
  ;; (with-eval-after-load 'org
  ;;   (setq org-src-tab-acts-natively t)
  ;;   (setq org-ref-notes-directory "~/Documents/reading/"
  ;;         org-ref-bibliography-notes "~/Documents/reading/notes/index.org"
  ;;         org-ref-default-bibliography '("~/Documents/reading/notes/index.bib")
  ;;         org-ref-pdf-directory "~/Documents/reading/lib/")
  ;;   (setq helm-bibtex-bibliography "~/Documents/reading/notes/index.bib"
  ;;         helm-bibtex-library-path "~/Documents/reading/lib/"
  ;;         helm-bibtex-notes-path "~/Documents/reading/notes/index.org"
  ;;         bibtex-completion-bibliography "~/Documents/reading/notes/index.bib"
  ;;         bibtex-completion-notes-path "~/Documents/reading/notes/index.org")
  ;;   )
  )
