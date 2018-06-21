;; configuration only for macOS systems
(defun jp/dotspacemacs/os-layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           (auto-completion (latex :variables
                                   auto-completion-enable-sort-by-usage t)
                            (haskell :variables haskell-completion-backend 'intero))
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
           (pdf :variables
                pdf-tools-handle-upgrades nil
                qpdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))
  (let ((local-additional-packages '(dash-at-point
                                     mac-pseudo-daemon
                                     google-c-style
                                     writeroom-mode
                                     ;; This next one is for yesod templates
                                     shakespeare-mode)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))


(defun jp/dotspacemacs/os-config ()
  "local configuration function.
This function is called at the very end of spacemacs initialization after layers configuration, after the general dotspacemacs/config
"
  ;; (setq exec-path-from-shell-arguments
  ;;       (delete "-i" exec-path-from-shell-arguments))

  ;; UI improvements specific to macOS
  (setq default-frame-alist '((ns-transparent-titlebar . t)
                              (ns-appearance . dark)))

  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

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
        org-journal-dir "~/Documents/org/journal/"
        org-journal-file-format "%Y%m%d"
        org-journal-date-prefix "#+TITLE: "
        org-journal-date-format "%A, %B %d %Y"
        ; The next two lines set new journal entries to be top level
        ; headlines and then tell org-journal to not insert the time
        ; as a headline
        org-journal-time-prefix "* "
        org-journal-time-format "")

  ;; org-ref configuration
  (setq reftex-default-bibliography '("~/Documents/reading/index.bib"))
  (setq org-ref-notes-directory "~/Documents/reading/"
        org-ref-bibliography-notes "~/Documents/reading/notes/index.org"
        org-ref-default-bibliography '("~/Documents/reading/index.bib")
        org-ref-pdf-directory "~/Documents/reading/lib/"
        )
  ;; setting up helm bibtex to match org-ref
  (setq helm-bibtex-bibliography "~/Documents/reading/references.bib"  ;; where your references are stored
        helm-bibtex-library-path "~/Documents/reading/lib/"            ;; where your pdfs etc are stored
        helm-bibtex-notes-path "~/Documents/reading/index.org"         ;; where your notes are stored
        bibtex-completion-bibliography "~/Documents/reading/index.bib" ;; writing completion
        bibtex-completion-notes-path "~/Documents/reading/index.org"
        )


  (global-unset-key (kbd "s-m"))
  (global-unset-key (kbd "s-q"))

  (custom-set-variables
   '(helm-ag-base-command "rg --no-heading"))


  ;; For emacs25
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (defconst pragmatapro-prettify-symbols-alist
    (mapcar (lambda (s)
              `(,(car s)
                .
                ,(vconcat
                  (apply 'vconcat (make-list (- (length (car s)) 1) (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                  (vector (decode-char 'ucs (cadr s))))))
            '(("[ERROR]"   #XE380)
              ("[DEBUG]"   #XE381)
              ("[INFO]"    #XE382)
              ("[WARN]"    #XE383)
              ("[WARNING]" #XE384)
              ("[ERR]"     #XE385)
              ("[FATAL]"   #XE386)
              ("[TRACE]"   #XE387)
              ("[FIXME]"   #XE388)
              ("[TODO]"    #XE389)
              ("[BUG]"     #XE38A)
              ("[NOTE]"    #XE38B)
              ("[HACK]"    #XE38C)
              ("[MARK]"    #XE38D)
              ("!!"        #XE900)
              ("!="        #XE901)
              ("!=="       #XE902)
              ("!!!"       #XE903)
              ("!≡"        #XE904)
              ("!≡≡"       #XE905)
              ("!>"        #XE906)
              ("#("        #XE920)
              ("#_"        #XE921)
              ("#{"        #XE922)
              ("#?"        #XE923)
              ("#>"        #XE924)
              ("##"        #XE925)
              ("%="        #XE930)
              ("%>"        #XE931)
              ("<~"        #XE932)
              ("&%"        #XE940)
              ("&&"        #XE941)
              ("&*"        #XE942)
              ("&+"        #XE943)
              ("&-"        #XE944)
              ("&/"        #XE945)
              ("&="        #XE946)
              ("&&&"       #XE947)
              ("&>"        #XE948)
              ("***"       #XE960)
              ("*="        #XE961)
              ("*/"        #XE962)
              ("*>"        #XE963)
              ("++"        #XE970)
              ("+++"       #XE971)
              ("+="        #XE972)
              ("+>"        #XE973)
              ("++="       #XE974)
              ("--"        #XE980)
              ("-<"        #XE981)
              ("-<<"       #XE982)
              ("-="        #XE983)
              ("->"        #XE984)
              ("->>"       #XE985)
              ("---"       #XE986)
              ("-->"       #XE987)
              ("-+-"       #XE988)
              ("-\\/"      #XE989)
              (".."        #XE990)
              ("..."       #XE991)
              ("..<"       #XE992)
              (".>"        #XE993)
              (".~"        #XE994)
              (".="        #XE995)
              ("/*"        #XE9A0)
              ("//"        #XE9A1)
              ("/>"        #XE9A2)
              ("/="        #XE9A3)
              ("/=="       #XE9A4)
              ("///"       #XE9A5)
              ("/**"       #XE9A6)
              ("::"        #XE9B0)
              (":="        #XE9B1)
              (":≡"        #XE9B2)
              (":>"        #XE9B3)
              (":=>"       #XE9B4)
              (":("        #XE9B5)
              (":-("       #XE9B6)
              (":)"        #XE9B7)
              (":-)"       #XE9B8)
              (":/"        #XE9B9)
              (":\\"       #XE9BA)
              (":3"        #XE9BB)
              (":D"        #XE9BC)
              (":P"        #XE9BD)
              (":>:"       #XE9BE)
              (":<:"       #XE9BF)
              ("<$>"       #XE9C0)
              ("<*"        #XE9C1)
              ("<*>"       #XE9C2)
              ("<+>"       #XE9C3)
              ("<-"        #XE9C4)
              ("<<"        #XE9C5)
              ("<<<"       #XE9C6)
              ("<<="       #XE9C7)
              ("<="        #XE9C8)
              ("<=>"       #XE9C9)
              ("<>"        #XE9CA)
              ("<|>"       #XE9CB)
              ("<<-"       #XE9CC)
              ("<|"        #XE9CD)
              ("<=<"       #XE9CE)
              ("<~"        #XE9CF)
              ("<~~"       #XE9D0)
              ("<<~"       #XE9D1)
              ("<$"        #XE9D2)
              ("<+"        #XE9D3)
              ("<!>"       #XE9D4)
              ("<@>"       #XE9D5)
              ("<#>"       #XE9D6)
              ("<%>"       #XE9D7)
              ("<^>"       #XE9D8)
              ("<&>"       #XE9D9)
              ("<?>"       #XE9DA)
              ("<.>"       #XE9DB)
              ("</>"       #XE9DC)
              ("<\\>"      #XE9DD)
              ("<\">"      #XE9DE)
              ("<:>"       #XE9DF)
              ("<~>"       #XE9E0)
              ("<**>"      #XE9E1)
              ("<<^"       #XE9E2)
              ("<!"        #XE9E3)
              ("<@"        #XE9E4)
              ("<#"        #XE9E5)
              ("<%"        #XE9E6)
              ("<^"        #XE9E7)
              ("<&"        #XE9E8)
              ("<?"        #XE9E9)
              ("<."        #XE9EA)
              ("</"        #XE9EB)
              ("<\\"       #XE9EC)
              ("<\""       #XE9ED)
              ("<:"        #XE9EE)
              ("<->"       #XE9EF)
              ("<!--"      #XE9F0)
              ("<--"       #XE9F1)
              ("<~<"       #XE9F2)
              ("<==>"      #XE9F3)
              ("==<"       #XEA00)
              ("=="        #XEA01)
              ("==="       #XEA02)
              ("==>"       #XEA03)
              ("=>"        #XEA04)
              ("=~"        #XEA05)
              ("=>>"       #XEA06)
              ("=/="       #XEA07)
              ("≡≡"        #XEA10)
              ("≡≡≡"       #XEA11)
              ("≡:≡"       #XEA12)
              (">-"        #XEA20)
              (">="        #XEA21)
              (">>"        #XEA22)
              (">>-"       #XEA23)
              (">>="       #XEA24)
              (">>>"       #XEA25)
              (">=>"       #XEA26)
              (">>^"       #XEA27)
              ("??"        #XEA40)
              ("?~"        #XEA41)
              ("?="        #XEA42)
              ("?>"        #XEA43)
              ("???"       #XEA44)
              ("^="        #XEA48)
              ("^."        #XEA49)
              ("^?"        #XEA4A)
              ("^.."       #XEA4B)
              ("^<<"       #XEA4C)
              ("^>>"       #XEA4D)
              ("^>"        #XEA4E)
              ("\\\\"      #XEA50)
              ("\\>"       #XEA51)
              ("\\/-"      #XEA52)
              ("@>"        #XEA57)
              ("|="        #XEA60)
              ("||"        #XEA61)
              ("|>"        #XEA62)
              ("|||"       #XEA63)
              ("|+|"       #XEA64)
              ("|->"       #XEA65)
              ("|-->"      #XEA66)
              ("|=>"       #XEA67)
              ("|==>"      #XEA68)
              ("~="        #XEA70)
              ("~>"        #XEA71)
              ("~~>"       #XEA72)
              ("~>>"       #XEA73)
              ("\">"       #XEA90))))

  (defun fp-prettify-symbols ()
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          ;; Need a way to make this work with comments as well.
          '(
  ;;           ("Int" . ?ℤ)
  ;;           ("Integer" . ?ℤ)
  ;;           ("Integral" . ?ℤ)
  ;;           ("Bool" . ?𝔹)
  ;;           ("String" . ?𝕊)
  ;;           ("ByteString" . ?𝕊)
  ;;           ("Text" . ?𝕊)
  ;;           ("Rational" . ?ℚ)
  ;;           ("Ratio" . ?ℚ)
  ;;           ("Real" . ?ℝ)
  ;;           ("Num" . ?ℝ)
  ;;           ("Double" . ?𝔻)
  ;;           ("Char" . ?ℂ)
  ;;           ("pi" . ?π)
  ;;           ("filter" . ?σ)
  ;;           ;;
  ;;           ("== " . (? (Br . Bl) ?≡ (Br . Bl) ? ))
  ;;           ("=:=" . (? (Br . Bl) ?≈ (Br . Bl) ? ))
  ;;           (">=" . ?)
  ;;           ("<=" . ?)
  ;;           ("=>" . ?)
  ;;           ("->" . ?)
  ;;           ("<-" . ?)
  ;;           ("::" . ?)
  ;;           ("/=" . ?)
  ;;           ;; ("|>" . ?)
  ;;           ("|>" . ?⪧)
  ;;           ;; ("<|" . ?⨞)
  ;;           ("<|" . ?⪦)
  ;;           ("++" . ?ꔠ)
  ;;           ("not" . ?¬)
  ;;           ("&&" . ?∧)
  ;;           ("||" . ?∨)
  ;;           ("~>" . ?)
  ;;           ("#(" . ?)
  ;;           ("#{" . ?)
  ;;           ("->>" . ?)
  ;;           (".." . ?)
  ;;           ("#_" . ?)
  ;;           (">>" . ?)
  ;;           ("..." . ?)
  ;;           ("<<" . ?)
  ;;           ("<<-" . ?)
  ;;           ("!!" . ?)
  ;;           (">>=" . ?)
  ;;           ("=<<" . ?)
  ;;           ("<=<" . ?)
  ;;           (">=>" . ?)
  ;;           ;; ("<|>" . ?)
  ;;           ;; ("<|>" . ?⦷)
  ;;           ("<|>" . ?ⵀ)
  ;;           ("<$>" . ?⨀)
  ;;           ("<+>" . ?⨁)
  ;;           ;; ("<*>" . ?⨷)
  ;;           ("<*>" . ?⨂)
  ;;           ("ap" . ?⨂)
  ;;           ;; ("<>" . ?)
            ("\\" . ?λ)
  ;;           ("\\" . ?☭)
  ;;           ("<." . ?≪)
  ;;           (".>" . ?≫)
            ("*" . ?⋅)
  ;;           ;; ("*" . ?×⋅)
  ;;           ;; ("*" . ?★)
            ("forall" . ?∀)
            ("forAll" . ?∀)
            ("all"    . ?∀)
            ("exists" . ?∃)
            ("undefined" . ?⊥)
  ;;           ("empty" . ?ⵁ)
  ;;           ("mempty" . ?ⵁ)
  ;;           ("mappend" . ?⨁)
  ;;           ("<>" . ?⨁)
            ("elem" . ?∈)
            ("flip elem" . ?∋)
            ("notElem" . ?∉)
            ("flip notElem" . ?∌)
            ("member" . ?∈)
            ("notMember" . ?∉)
            ("union" . ?⋃)
            ("intersection" . ?⋂)
            ("isSubsetOf" . ?⊆)
            ("isProperSubsetOf" . ?⊂)
            (" . " . (? (Br . Bl) ?◦ (Br . Bl) ? ))
            ("/" . ?÷)
            ("div" . ?÷)
            ("quot" . ?÷)
  ;;           ("--" . ?)
  ;;           (":=" . ?)
            )))

  (add-hook 'haskell-mode-hook #'fp-prettify-symbols)

  (defun add-pragmatapro-prettify-symbols-alist ()
    (dolist (alias pragmatapro-prettify-symbols-alist)
      (push alias prettify-symbols-alist)))

  (add-hook 'prog-mode-hook
            #'add-pragmatapro-prettify-symbols-alist)

  (global-prettify-symbols-mode +1))
