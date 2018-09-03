;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the server
(server-start)
;; make Emacs.app open files in an existing frame instead of a new frame
(setq ns-pop-up-frames nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change stack depth, someone is going nuts
(setq max-lisp-eval-depth 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace yes or no with y or n
;; (defun yes-or-no-p@maybe-just-y-or-n-p (orig-fun prompt)
;;   (funcall
;;    (if (eq this-command 'kill-buffer)
;;        #'y-or-n-p
;;      #'yes-or-no-p)
;;    prompt))

;; (advice-add 'yes-or-no-p :around #'yes-or-no-p@maybe-just-y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font ligatures
;;
;; TODO Replace this with a check that either loads mac-auto-operator-composition-mode
;;      -OR- uses prettify symbols
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(set-frame-font "PragmataPro 16" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable smooth scrolling
(if mac-mouse-wheel-smooth-scroll
    (setq mac-mouse-wheel-smooth-scroll nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keys
;;
;; zoooooooooom
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;; Move the entire buffer up or down one line at a time.
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
;; Disables suspend keys so we aren't locking up emacs in a GUI
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp settings
;;
;; ZSH on the PDX servers caused problems with tramp hanging.
;; Force to bash instead to make life simple.
;;
;; Mon Jun 19 17:43:56 PDT 2017
;; I'm not sure but this _might_ be causing problems for my local TRAMP mode
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode
;;
;; Enables logging completion as well as moving a task to WAIT
(require 'org)
(require 'org-install)
(add-to-list 'org-modules 'org-habit)



(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-habit-graphs-everywhere t                                            ;; Configuring display of org-habit in the agenda buffer
      org-habit-graph-column 80
      org-habit-preceding-days 14
      org-habit-show-habits-only-for-today nil
      org-directory "~/Documents/org/"
      org-src-tab-acts-natively t
      org-reverse-note-order t
      ;; diary is a 0 length file to keep emacs happy
      diary-file "~/Documents/org/diary"
      org-default-notes-file "~/Documents/org/notes.org"
      jp/org-inbox-file "~/Documents/org/inbox.org"
      org-agenda-files '("~/Documents/org/agenda.org"                          ;; Some kind of actual agenda
                         "~/Documents/org/inbox.org"                           ;; A dumping ground
                         "~/Documents/org/index.org"                           ;; The larger org-mode project list and general purpose index
                         "~/Documents/org/geu.org"                             ;; agitating for labor
                         "~/Documents/org/333.org"                             ;; CS333 - fun for you, fun for me
                         "~/Documents/org/calsync/jeremiahpeschka-cal.org"
                         "~/Documents/org/calsync/legitbiz-cal.org"
                         "~/Documents/org/calsync/jpeschka-cal.org")
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-ndays 7
      org-agenda-show-all-dates t
      org-agenda-use-time-grid t
      org-deadline-warning-days 14
      org-journal-dir "~/Documents/org/journal/"
      org-journal-file-format "%Y%m%d"
      org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%A, %B %d %Y"
      ;; The next two lines set new journal entries to be top level
      ;; headlines and then tell org-journal to not insert the time
      ;; as a headline
      org-journal-time-prefix "* "
      org-journal-time-format ""
      ;; Include agenda archive files when searching
      org-agenda-text-search-extra-files (quote (agenda-archives))
      ;; sets org-refile to be able to work with any org-agenda-files
      org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
;; Custom agenda views
(setq org-agenda-custom-commands
      `(("u" "Unscheduled TODO items"
         (lambda (org-match)
           (find-file org-todo-file)
           (org-match-sparse-tree 'todo-only "-DEADLINE={.}/!")))
        ("t" "All TODO items"
         (lambda (org-match)
           (find-file org-todo-file)
           (org-match-sparse-tree 'todo-only "/!")))
        ("a" "Agenda" agenda)))

;; Start on the current day, not Monday.
(setq org-agenda-start-on-weekday nil)

;; Set up notifications for org
(require 'appt)
(setq appt-time-msg-list nil          ;; clear existing appt list
      appt-display-interval '10       ;; warn every 10 minutes from t - appt-message-warning-time
      appt-message-warning-time '10   ;; sent first warning 10 minutes before appointment
      appt-display-mode-line nil      ;; don't show in the modeline
      appt-display-format 'window)    ;; passes notifications to the designated window function-key-map
(appt-activate 1)                     ;; activate appointment notification
(display-time)                        ;; activate time display

(org-agenda-to-appt)                  ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; set up the call to terminal-notifier
(defvar my-notifier-path
  "/usr/local/bin/terminal-notifier")
(defun my-appt-send-notification (title msg)
  (shell-command (concat my-notifier-path " -message " msg " -title " title " -sender org.gnu.Emacs ")))

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification
   (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))


;; org-ref configuration
(setq reftex-default-bibliography '("~/Documents/reading/index.bib"))
(setq org-ref-notes-directory "~/Documents/reading/"
      org-ref-bibliography-notes "~/Documents/reading/notes/index.org"
      org-ref-default-bibliography '("~/Documents/reading/index.bib")
      org-ref-pdf-directory "~/Documents/reading/lib/")


;; t - Prompt for a title and then add to notes.org unless you refile it
(setq org-capture-templates
      '(("t" "todo" entry (file jp/org-inbox-file)
         "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file org-default-notes-file)
         "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
        ("i" "Idea" entry (file jp/org-inbox-file)
         "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
        ("n" "Next Task" entry (file+headline org-inbox-file "Tasks")
         "** NEXT %? \nDEADLINE: %t")))

(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-q"))

(defun update-org-src-locs ()
  (when (string= major-mode "org-mode")
    (save-excursion
      (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (hl)
                         (goto-char (org-element-property :begin hl))
                         (forward-line -1)
                         (when (string= (buffer-substring-no-properties (point) (line-end-position))
                                        "#+END_SRC")
                           (forward-line)
                           (insert "\n")))))))

(add-hook 'after-save-hook 'update-org-src-locs)

(defun jp/org-template ()
  (insert "#+AUTHOR: Jeremiah Peschka
#+EMAIL: jeremiah.peschka@gmail.com
#+STARTUP: indent showall
#+OPTIONS: tags:nil")
  (org-mode-restart))

(define-auto-insert "\\.org$" #'jp/org-template)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp goodness
(defun close-all-parentheses ()
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
                 (progn
                   (backward-up-list)
                   (let ((syntax (syntax-after (point))))
                     (case (car syntax)
                           ((4) (setq closing (cons (cdr syntax) closing)))
                           ((7 8) (setq closing (cons (char-after (point)) closing)))))
                   t)
               ((scan-error) nil))))
    (apply #'insert (nreverse closing))))

(define-key global-map (kbd "C-c [") 'close-all-parentheses)

;; trigger whitespace-cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)



(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizing radian
(defun radian-local--after-init ()
  (interactive)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ivy-bibtex
  (straight-use-packate 'ivy-bibtex)
  ;; setting up helm bibtex to match org-ref
  (setq bibtex-completion-bibliography "~/Documents/reading/index.bib" ;; writing completion
        bibtex-completion-library-path '("~/Documents/reading/lib/")
        bibtex-completion-notes-path "~/Documents/reading/index.org"
        bibtex-completion-pdf-field "file"
        )

  (straight-use-package 'rainbow-delimiters)
  ;;(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  ;;(add-hook 'rust-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;(straight-use-package 'mac-pseudo-daemon)
  ;(mac-pseudo-daemon-mode t)


  (straight-use-package 'dash-at-point)
  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset)

  ;; sets up c and C++ programming environment
  (straight-use-package 'google-c-style)
  (setq c-basic-offset 2)
  (add-hook 'c-mode-common-hook 'google-set-c-style)

  ;; Adds writeroom-mode
  (straight-use-package 'writeroom-mode)


  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark red"))))
   '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange red"))))
   '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "gold"))))
   '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "lime green"))))
   '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "cornflower blue"))))
   '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark violet")))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MOAR ORG
  ;;
  ;; org-journal gives C-c C-j to create a new journal entry
  (straight-use-package 'org-journal)
  (straight-use-package 'org-plus-contrib)
  ;; pretty bullets
  (straight-use-package 'org-bullets)
  ;; always pretty bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


  (straight-use-package 'hydra)

  (defun radian-insert-date ()
    "Insert the current date in ISO 8601 format."
    (interactive)
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S" nil t)))

  ;; maybe we'll fix challenger deep some day...
  ;; (setq radian-color-theme-enable nil)
  ;; (straight-use-package 'challenger-deep-theme)
  ;; (load-theme 'challenger-deep t)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Remove platform keybindings so we don't accidentally close emacs
  (unbind-key "s-q")
  (unbind-key "s-w")
  (unbind-key "s-c")
  (unbind-key "s-r")

  ;; Also add some sane keybindings
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Better searching through ripgrep
  (straight-use-package 'projectile-ripgrep)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; haskell
  (add-to-list 'load-path "~/src/lsp-haskell")
  (add-to-list 'load-path "~/src/lsp-mode")
  (add-to-list 'load-path "~/src/lsp-ui")

  (require 'lsp-ui)
  (require 'lsp-haskell)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (setq lsp-haskell-process-path-hie "hie-wrapper")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pdf-tools
  (straight-use-package 'pdf-tools)
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ace-window
  (straight-use-package 'ace-window)

  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u (lambda ()
                (progn
                  (winner-undo)
                  (setq this-command 'winner-undo))))
          (?r winner-redo)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; HYDRAS
  ;;
  ;; Ace-window hydra controls - for the lazy
  ;; Trigger with 'M-o w'
  (defhydra hydra-window ()
    "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←           _v_ertical      _b_uffer		_q_ X←
_j_ ↓           _x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑           _z_ undo        _a_ce 1		_e_ X↑
_l_ →           _Z_ reset       _s_wap		_r_ X→
_F_ollow		_D_lt Other     _S_ave		max_i_mize
_SPC_ cancel	_o_nly this     _d_elete
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("q" hydra-move-splitter-left)
    ("w" hydra-move-splitter-down)
    ("e" hydra-move-splitter-up)
    ("r" hydra-move-splitter-right)
    ("b" helm-mini)
    ("f" helm-find-files)
    ("F" follow-mode)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("o" delete-other-windows)
    ("i" ace-maximize-window)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("Z" winner-redo)
    ("SPC" nil))
  (add-to-list 'aw-dispatch-alist '(?w hydra-window/body) t)
  (global-set-key (kbd "M-o") 'ace-window)

  ;; Hydra for buffer o' buffers
  ;; Just press '.' to bring it up
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
"
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("t" ibuffer-toggle-marks)

    ("D" ibuffer-do-delete)
    ("s" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("S" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("H" describe-mode :color blue)

    ("h" ibuffer-backward-filter-group)
    ("k" ibuffer-backward-line)
    ("l" ibuffer-forward-filter-group)
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)

    ("TAB" ibuffer-toggle-filter-group)

    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" quit-window :color blue)
    ("." nil :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  ;; toggle the hydra with '.'
  (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
  ;; Automatically open the hydra when ibuffer opens
  (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

  (setq magit-repository-directories '("~/src/"))
  (global-git-commit-mode t)

  (custom-set-variables
   '(menu-bar-mode t)))

(add-hook 'radian-after-init-hook
          #'radian-local--after-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty symbols
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
            ("TODO"      #XE389)
            ("[BUG]"     #XE38A)
            ("[NOTE]"    #XE38B)
            ("[HACK]"    #XE38C)
            ("[MARK]"    #XE38D)
            (":/"        #XE9B9)
            (":\\"       #XE9BA)
            (":3"        #XE9BB)
            (":D"        #XE9BC)
            (":P"        #XE9BD)
            (":>:"       #XE9BE)
            (":<:"       #XE9BF)
            ("<\\>"      #XE9DD)
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
            ("\\/-"      #XEA52))))

(defun fp-prettify-symbols ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        ;; Need a way to make this work with comments as well.
        '(
          ("\\" . ?λ)
          ("*" . ?⋅)
          ("forall" . ?∀)
          ("forAll" . ?∀)
          ("all"    . ?∀)
          ("exists" . ?∃)
          ("undefined" . ?⊥)
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
          ("quot" . ?÷))))

(add-hook 'haskell-mode-hook #'fp-prettify-symbols)

(defun add-pragmatapro-prettify-symbols-alist ()
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

(add-hook 'prog-mode-hook
          #'add-pragmatapro-prettify-symbols-alist)

(global-prettify-symbols-mode +1)
