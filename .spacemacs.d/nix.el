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
           latex
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

           pdf-tools
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

(defun jp/dotspacemacs/system-init()
  "local initialization function
This function is called at the end of dotspacemacs/init.
Put any OS-specific variable modification required in here."
  (if (file-accessible-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")))


(defun jp/dotspacemacs/config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"
  ;; sets up c and C++ programming environment
  (setq c-basic-offset 2)
  (add-hook 'c-mode-common-hook 'google-set-c-style)

  (setq magit-repository-directories '("~/src/"))

  ;; pdx.edu system configuration
  ;; TODO This needs to be changed to detect console mode emacs
  (when (string-match ".*cs\.pdx\.edu" system-name)
    (progn
      (require 'disable-mouse)
      (global-disable-mouse-mode)))

  (setq org-directory "~/Documents/org/")
  (with-eval-after-load 'org
    (setq org-src-tab-acts-natively t)
    (setq org-ref-notes-directory "~/Documents/reading/"
          org-ref-bibliography-notes "~/Documents/reading/notes/index.org"
          org-ref-default-bibliography '("~/Documents/reading/notes/index.bib")
          org-ref-pdf-directory "~/Documents/reading/lib/")
    (setq helm-bibtex-bibliography "~/Documents/reading/notes/index.bib"
          helm-bibtex-library-path "~/Documents/reading/lib/"
          helm-bibtex-notes-path "~/Documents/reading/notes/index.org"
          bibtex-completion-bibliography "~/Documents/reading/notes/index.bib"
          bibtex-completion-notes-path "~/Documents/reading/notes/index.org")
    )
  )
