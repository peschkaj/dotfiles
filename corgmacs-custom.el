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
                          (concat org-directory "geu.org")                  ;; agitating for labor
                          (concat org-directory "333.org")                  ;; CS333 - fun for you, fun for me
                          (concat org-directory "calsync/legitbiz-cal.org")
                          (concat org-directory "calsync/personal.org")
                          (concat org-directory "calsync/pdx.edu.org"))))

(let ((jp/calsync
       (expand-file-name "calendar-sync.el" "~/src/peschkaj/seekrets/emacs")))
  (if (file-readable-p jp/calsync)
      (load-file jp/calsync)))

;;; corgmacs-custom.el ends here
