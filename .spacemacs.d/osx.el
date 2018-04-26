;; configuration only for macOS systems
(defun jp/dotspacemacs/os-layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           (auto-completion (latex :variables
                                   auto-completion-enable-sort-by-usage t)
                            (haskell :variables
                                     haskell-completion-backend 'intero))
           (c-c++ :variables
                  c-c++-default-mode-for-headers 'c++-mode
                  c-c++-enable-clang-support t
                  clang-format-style "file"
                  )
           latex
           (python :variables
                   python-enable-yapf-format-on-save t
                   flycheck-python-pycompile-executable "python3")
           haskell
           ;; Moves pdf-tools to be managed by the operating system
           ;; See https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx#22591 for additional details on this configuration
           (pdf-tools :variables
                      pdf-tools-handle-upgrades nil
                      pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))
  (let ((local-additional-packages '(dash-at-point
                                     mac-pseudo-daemon
                                     google-c-style
                                     writeroom-mode)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))


(defun jp/dotspacemacs/os-config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"

  ;; Sets shell-mode to use zsh, no matter where it lives
  (setq shell-file-name "zsh")

  ;; we don't find the correct tex distribution for some reason
  (add-to-list 'exec-path "/usr/local/texlive/2017/bin/x86_64-darwin")
  (add-to-list 'exec-path "/Library/TeX/texbin/")

  (setq-default TeX-engine 'xetex)

  (mac-pseudo-daemon-mode t)

  ;; (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  ;; (add-to-list 'exec-path "/Library/TeX/texbin/")
  ;; Don't forget to symlink pdftex
  ;; ln -s /Library/TeX/texbin/pdftex /usr/local/bin/pdflatex

  ;; OS X ls doesn't support --dired flag
  (setq dired-use-ls-dired nil)

  (setq-default mac-right-option-modifier nil)
  (setq magit-repository-directories '("~/src/"))

  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset)

  ;; sets up c and C++ programming environment
  (setq c-basic-offset 2)
  (add-hook 'c-mode-common-hook 'google-set-c-style)


  (setq org-directory "~/Documents/org/"
        org-src-tab-acts-natively t
        org-reverse-note-order t
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files '("~/Documents/org/agenda.org")
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-ndays 7
        org-agenda-show-all-dates t
        org-deadline-warning-days 14
        org-journal-dir "~/Documents/org/journal/")

  (defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y%m%d")))
      (expand-file-name (concat org-journal-dir daily-name ".org"))))

  (defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (global-set-key (kbd "C-c f j") 'journal-file-today)

  (setq org-capture-templates
        '(
          ("j" "Journal Entry"
           entry (file+datetree get-journal-file-today)
           "* Event: %?\n\n  %i\n\n  From: %a"
           :empty-lines 1)))

  ;; Journal related navigation
  (defun split-string-with-number (string)
    "Returns a list of three components of the string, the first is
the text prior to any numbers, the second is the embedded number,
and the third is the rest of the text in the string."
    (let* ((start (string-match "[0-9]+" string))
           (end (string-match "[^0-9]+" string start)))
      (if start
          (list (substring string 0 start)
                (substring string start end)
                (if end  (substring string end)  "")))))

  (defun find-file-number-change (f)
    (let* ((filename (buffer-file-name))
           (parts    (split-string-with-number
                      (file-name-base filename)))
           (new-name (number-to-string
                      (funcall f (string-to-number (nth 1 parts))))))
      (concat (file-name-directory filename)
              (nth 0 parts)
              new-name
              (file-name-extension filename t))))

  (defun find-file-increment ()
    "Takes the current buffer, and loads the file that is 'one
more' than the file contained in the current buffer. This
requires that the current file contain a number that can be
incremented."
    (interactive)
    (find-file (find-file-number-change '1+)))

  (defun find-file-decrement ()
    "Takes the current buffer, and loads the file that is 'one
less' than the file contained in the current buffer. This
requires that the current file contain a number that can be
decremented."
    (interactive)
    (find-file (find-file-number-change '1-)))

  (global-set-key (kbd "C-c f +") 'find-file-increment)
  (global-set-key (kbd "C-c f n") 'find-file-increment)
  (global-set-key (kbd "C-c f -") 'find-file-decrement)
  (global-set-key (kbd "C-c f p") 'find-file-decrement)
  (global-unset-key (kbd "s-m"))
  (global-unset-key (kbd "s-q"))

  (custom-set-variables
   '(helm-ag-base-command "rg --no-heading"))

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
