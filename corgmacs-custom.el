;;; package --- Summary
;;; Commentary:
;;;  Custom settings for whatever.
;;; Code:
;;;  code goes here, moron
(defun corgmacs/set-org-agenda-files ()
  "All variables and functions might as well have a documentation string."
  (setq org-agenda-files (list
                          (concat org-directory "agenda.org")               ;; Some kind of actual agenda
                          (concat org-directory "inbox.org")                ;; A dumping ground
                          (concat org-directory "index.org")                ;; The larger org-mode project list and general purpose index
                          )))

(let ((jp/calsync
       (expand-file-name "calendar-sync.el" "~/src/peschkaj/seekrets/emacs")))
  (if (file-readable-p jp/calsync)
      (load-file jp/calsync)))

(use-package paradox
  :ensure t
  :config (paradox-enable))

(load-theme 'zerodark t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e configuration
(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-maildir "/home/jeremiah/.mail"
        mail-user-agent 'mu4e-user-agent
        mu4e-view-show-images t
        ;mu4e-html2text-command "w3m -T text/html"
        mu4e-html2text-command "pandoc -f html -t markdown "
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-use-fancy-chars t
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-folder "/sent"
        mu4e-drafts-folder "/drafts"
        mu4e-completing-read-function 'completing-read

        ;; please close messages after sending...
        mu4e-conversation-kill-buffer-on-exit t
        message-kill-buffer-on-exit t

        ;; Pick the first context on opening mu4e
        mu4e-context-policy 'pick-first

        ;; nobody needs to confirm quitting
        mu4e-confirm-quit nil

        ;; msmtp magical incantation
        message-sendmail-f-is-evil 't
        )
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; Choose account label to feed msmtp -a option based on From header
  ;; in Message buffer; This function must be added to
  ;; message-send-mail-hook for on-the-fly change of From address before
  ;; sending message since message-send-mail-hook is processed right
  ;; before sending message.
  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "jeremiah@legit.biz" from) "legit.biz")
                 ((string-match "jpeschka@pdx.edu" from) "pdx.edu"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  (add-hook 'message-send-mail-hook 'choose-msmtp-account))

;; Configure sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Jeremiah Peschka")


;; (use-package async
;;   :ensure t
;;   :config
;;   (require 'smtpmail-async)
;;   (setq
;;    ;; Configure async mail sending
;;    send-mail-function 'async-smtpmail-send-it
;;    message-send-mail-function 'async-smtpmail-send-it))
;; (defun async-smtpmail-send-queued-mail (sync-func &rest args)
;; "Send email without locking up Emacs. Argument SYNC-FUNC and ARGS should appear here."
;;   (message "Starting asynchronous smtpmail-send-queued-mail")
;;   (async-start
;;    `(lambda ()
;;       (require 'smtpmail)
;;       ;; see smtpmail-async.el - we inject the same variables
;;       ,(async-inject-variables
;;         "\\`\\(smtpmail\\|async-smtpmail\\|\\(user-\\)?mail\\)-\\|auth-sources\\|epg\\|nsm"
;;         nil
;;         "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
;;       ;; if we don't use the above inject we can pass in specific variables like this:
;;       ;; (setq smtpmail-queue-dir ,smtpmail-queue-dir)
;;       ;; (setq smtpmail-smtp-server ,smtpmail-smtp-server)
;;       (,sync-func))
;;    (lambda (&optional _unused)
;;      (message "Done sending queued mail in the background."))))

;; https://emacs.stackexchange.com/a/14827/8743 has more, err, advice.
;; (advice-add #'smtpmail-send-queued-mail :around #'async-smtpmail-send-queued-mail)

(use-package helm-mu
  :ensure t
  :after mu4e)

(use-package mu4e-conversation
  :ensure t
  :after mu4e
  :config
  (global-mu4e-conversation-mode)
  (setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)
  (add-hook 'mu4e-conversation-hook 'flyspell-mode))

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "legit.biz"
           :match-func (lambda (msg)
                         (when msg (string-prefix-p "/legit.biz" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-trash-folder . "/legit.biz/Trash")
                   (mu43-reflie-folder . "/legit.biz/Archive")))
         ,(make-mu4e-context
           :name "pdx.edu"
           :match-func (lambda (msg)
                         (when msg (string-prefix-p "/pdx.edu" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-trash-folder . "/pdx.edu/[Gmail].Trash")
                   (mu4e-refile-folder . "/pdx.edu/[Gmail].Archive")
                   ))
         ))

(defvar my-mu4e-account-alist
  '(("legit.biz"
     (mu4e-sent-folder "/legit.biz/Sent")
     (user-mail-address "jeremiah@legit.biz")
     (send-mail-function 'sendmail-send-it)
     (sendmail-program "/usr/bin/msmtp")
     (user-full-name "Jeremiah Peschka")
     (mu4e-compose-signature (concat "--------------------\n"
                                     "Jeremiah Peschka\n"))
     )
    ("pdx.edu"
     (mu4e-sent-folder "/pdx.edu/Sent")
     (user-email-address "jpeschka@pdx.edu")
     (send-mail-function 'sendmail-send-it)
     (sendmail-program "/usr/bin/msmtp")
     (user-full-name "Jeremiah Peschka")
     (mu4e-compose-signature (concat "Jeremiah Peschka\n"
                                     "---------------------------\n"
                                     "PhD Student\n"
                                     "Computer Science Department\n")))))


(defun my-mu4e-set-account ()
"Set the account for composing a message.
This function is taken from:
https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; The following convinces offlineimap to really really delete things that we've
;; already deleted inside of mu4e
(defun remove-nth-element (nth list)
"Remove the NTH element of a LIST."
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(setq mu4e-marks (remove-nth-element 5 mu4e-marks))

(add-to-list 'mu4e-marks
             '(trash
               :char ("d" . "▼")
               :prompt "dtrash"
               :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
               :action (lambda (docid msg target)
                         (mu4e~proc-move docid
                                         (mu4e~mark-check-target target) "-N"))))

;; Include a bookmark to open all of my inboxes
(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "All Inboxes"
        :query "maildir:/legit.biz/INBOX OR maildir:/pdx.edu/INBOX"
        :key ?i))

(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "legit.biz Inbox"
        :query "maildir:/legit.biz/INBOX"
        :key ?l))

(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "pdx.edu Inbox"
        :query "maildir:/pdx.edu/INBOX"
        :key ?p))


;;; corgmacs-custom.el ends here
