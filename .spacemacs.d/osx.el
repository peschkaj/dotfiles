;; configuration only for macOS systems
(defun jp/dotspacemacs/os-layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           (auto-completion (latex)
                            :variables
                            auto-completion-enable-sort-by-usage t)
           common-lisp
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
                                     mac-pseudo-daemon)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))


(defun jp/dotspacemacs/os-config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"
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

  ;; Fira code
  ;;
  ;; Fira Code Symbol can be obtained from https://github.com/tonsky/FiraCode/issues/211
  ;; This configuration is sourced from https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
  ;;
  ;; This works when using emacs --daemon + emacsclient
  ;; (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
  ;; ;; This works when using emacs without server/client
  ;; (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  ;; ;; I haven't found one statement that makes both of the above situations work, so I use both for now

  ;; (defconst fira-code-font-lock-keywords-alist
  ;;   (mapcar (lambda (regex-char-pair)
  ;;             `(,(car regex-char-pair)
  ;;               (0 (prog1 ()
  ;;                    (compose-region (match-beginning 1)
  ;;                                    (match-end 1)
  ;;                                    ;; The first argument to concat is a string containing a literal tab
  ;;                                    ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
  ;;           '(("\\(www\\)"                   #Xe100)
  ;;             ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
  ;;             ("\\(\\*\\*\\*\\)"             #Xe102)
  ;;             ("\\(\\*\\*/\\)"               #Xe103)
  ;;             ("\\(\\*>\\)"                  #Xe104)
  ;;             ("[^*]\\(\\*/\\)"              #Xe105)
  ;;             ("\\(\\\\\\\\\\)"              #Xe106)
  ;;             ("\\(\\\\\\\\\\\\\\)"          #Xe107)
  ;;             ("\\({-\\)"                    #Xe108)
  ;;             ("\\(\\[\\]\\)"                #Xe109)
  ;;             ("\\(::\\)"                    #Xe10a)
  ;;             ("\\(:::\\)"                   #Xe10b)
  ;;             ("[^=]\\(:=\\)"                #Xe10c)
  ;;             ("\\(!!\\)"                    #Xe10d)
  ;;             ("\\(!=\\)"                    #Xe10e)
  ;;             ("\\(!==\\)"                   #Xe10f)
  ;;             ("\\(-}\\)"                    #Xe110)
  ;;             ("\\(--\\)"                    #Xe111)
  ;;             ("\\(---\\)"                   #Xe112)
  ;;             ("\\(-->\\)"                   #Xe113)
  ;;             ("[^-]\\(->\\)"                #Xe114)
  ;;             ("\\(->>\\)"                   #Xe115)
  ;;             ("\\(-<\\)"                    #Xe116)
  ;;             ("\\(-<<\\)"                   #Xe117)
  ;;             ("\\(-~\\)"                    #Xe118)
  ;;             ("\\(#{\\)"                    #Xe119)
  ;;             ("\\(#\\[\\)"                  #Xe11a)
  ;;             ("\\(##\\)"                    #Xe11b)
  ;;             ("\\(###\\)"                   #Xe11c)
  ;;             ("\\(####\\)"                  #Xe11d)
  ;;             ("\\(#(\\)"                    #Xe11e)
  ;;             ("\\(#\\?\\)"                  #Xe11f)
  ;;             ("\\(#_\\)"                    #Xe120)
  ;;             ("\\(#_(\\)"                   #Xe121)
  ;;             ("\\(\\.-\\)"                  #Xe122)
  ;;             ("\\(\\.=\\)"                  #Xe123)
  ;;             ("\\(\\.\\.\\)"                #Xe124)
  ;;             ("\\(\\.\\.<\\)"               #Xe125)
  ;;             ("\\(\\.\\.\\.\\)"             #Xe126)
  ;;             ("\\(\\?=\\)"                  #Xe127)
  ;;             ("\\(\\?\\?\\)"                #Xe128)
  ;;             ("\\(;;\\)"                    #Xe129)
  ;;             ("\\(/\\*\\)"                  #Xe12a)
  ;;             ("\\(/\\*\\*\\)"               #Xe12b)
  ;;             ("\\(/=\\)"                    #Xe12c)
  ;;             ("\\(/==\\)"                   #Xe12d)
  ;;             ("\\(/>\\)"                    #Xe12e)
  ;;             ("\\(//\\)"                    #Xe12f)
  ;;             ("\\(///\\)"                   #Xe130)
  ;;             ("\\(&&\\)"                    #Xe131)
  ;;             ("\\(||\\)"                    #Xe132)
  ;;             ("\\(||=\\)"                   #Xe133)
  ;;             ("[^|]\\(|=\\)"                #Xe134)
  ;;             ("\\(|>\\)"                    #Xe135)
  ;;             ("\\(\\^=\\)"                  #Xe136)
  ;;             ("\\(\\$>\\)"                  #Xe137)
  ;;             ("\\(\\+\\+\\)"                #Xe138)
  ;;             ("\\(\\+\\+\\+\\)"             #Xe139)
  ;;             ("\\(\\+>\\)"                  #Xe13a)
  ;;             ("\\(=:=\\)"                   #Xe13b)
  ;;             ("[^!/]\\(==\\)[^>]"           #Xe13c)
  ;;             ("\\(===\\)"                   #Xe13d)
  ;;             ("\\(==>\\)"                   #Xe13e)
  ;;             ("[^=]\\(=>\\)"                #Xe13f)
  ;;             ("\\(=>>\\)"                   #Xe140)
  ;;             ("\\(<=\\)"                    #Xe141)
  ;;             ("\\(=<<\\)"                   #Xe142)
  ;;             ("\\(=/=\\)"                   #Xe143)
  ;;             ("\\(>-\\)"                    #Xe144)
  ;;             ("\\(>=\\)"                    #Xe145)
  ;;             ("\\(>=>\\)"                   #Xe146)
  ;;             ("[^-=]\\(>>\\)"               #Xe147)
  ;;             ("\\(>>-\\)"                   #Xe148)
  ;;             ("\\(>>=\\)"                   #Xe149)
  ;;             ("\\(>>>\\)"                   #Xe14a)
  ;;             ("\\(<\\*\\)"                  #Xe14b)
  ;;             ("\\(<\\*>\\)"                 #Xe14c)
  ;;             ("\\(<|\\)"                    #Xe14d)
  ;;             ("\\(<|>\\)"                   #Xe14e)
  ;;             ("\\(<\\$\\)"                  #Xe14f)
  ;;             ("\\(<\\$>\\)"                 #Xe150)
  ;;             ("\\(<!--\\)"                  #Xe151)
  ;;             ("\\(<-\\)"                    #Xe152)
  ;;             ("\\(<--\\)"                   #Xe153)
  ;;             ("\\(<->\\)"                   #Xe154)
  ;;             ("\\(<\\+\\)"                  #Xe155)
  ;;             ("\\(<\\+>\\)"                 #Xe156)
  ;;             ("\\(<=\\)"                    #Xe157)
  ;;             ("\\(<==\\)"                   #Xe158)
  ;;             ("\\(<=>\\)"                   #Xe159)
  ;;             ("\\(<=<\\)"                   #Xe15a)
  ;;             ("\\(<>\\)"                    #Xe15b)
  ;;             ("[^-=]\\(<<\\)"               #Xe15c)
  ;;             ("\\(<<-\\)"                   #Xe15d)
  ;;             ("\\(<<=\\)"                   #Xe15e)
  ;;             ("\\(<<<\\)"                   #Xe15f)
  ;;             ("\\(<~\\)"                    #Xe160)
  ;;             ("\\(<~~\\)"                   #Xe161)
  ;;             ("\\(</\\)"                    #Xe162)
  ;;             ("\\(</>\\)"                   #Xe163)
  ;;             ("\\(~@\\)"                    #Xe164)
  ;;             ("\\(~-\\)"                    #Xe165)
  ;;             ("\\(~=\\)"                    #Xe166)
  ;;             ("\\(~>\\)"                    #Xe167)
  ;;             ("[^<]\\(~~\\)"                #Xe168)
  ;;             ("\\(~~>\\)"                   #Xe169)
  ;;             ("\\(%%\\)"                    #Xe16a)
  ;;             ;; ("\\(x\\)"                   #Xe16b) This ended up Most of the datbeing hard to do properly so i'm leaving it out.
  ;;             ("[^:=]\\(:\\)[^:=]"           #Xe16c)
  ;;             ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
  ;;             ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

  ;; (defun add-fira-code-symbol-keywords ()
  ;;   (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

  ;; (add-hook 'prog-mode-hook
  ;;           #'add-fira-code-symbol-keywords)


  (setq org-directory "~/Documents/org/"
        org-src-tab-acts-natively t
        org-reverse-note-order t
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files '("~/Documents/org/todo.org")
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
           entry (file+datetree (get-journal-file-today))
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
