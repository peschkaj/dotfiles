;;; keybindings.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Wieland Hoffmann

;; Author: Wieland Hoffmann <wieland@mineo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defconst mineo-rtags-overrides
  '(("C-]" 'rtags-find-symbol-at-point)
    ("M-." 'rtags-find-symbol-at-point)))

(defun mineo-rtags-set-evil-keys ()
  (dolist (override mineo-rtags-overrides)
    (evil-local-set-key 'normal (car override) (cdr override))))

(add-hook 'c-mode-common-hook 'mineo-rtags-set-evil-keys)

;;; https://github.com/mheathr/spacemacs/blob/develop/contrib/!lang/c-c%2B%2B/packages.el
(dolist (mode '(c-mode c++-mode))
  (evil-leader/set-key-for-mode mode
    "R." 'rtags-find-symbol-at-point
    "R," 'rtags-find-references-at-point
    "Rv" 'rtags-find-virtuals-at-point
    "RV" 'rtags-print-enum-value-at-point
    "R/" 'rtags-find-all-references-at-point
    "RY" 'rtags-cycle-overlays-on-screen
    "R>" 'rtags-find-symbol
    "R<" 'rtags-find-references
    "R[" 'rtags-location-stack-back
    "R]" 'rtags-location-stack-forward
    "RD" 'rtags-diagnostics
    "RG" 'rtags-guess-function-at-point
    "Rp" 'rtags-set-current-project
    "RP" 'rtags-print-dependencies
    "Re" 'rtags-reparse-file
    "RE" 'rtags-preprocess-file
    "RR" 'rtags-rename-symbol
    "RM" 'rtags-symbol-info
    "RS" 'rtags-display-summary
    "RO" 'rtags-goto-offset
    "R;" 'rtags-find-file
    "RF" 'rtags-fixit
    "RL" 'rtags-copy-and-print-current-location
    "RX" 'rtags-fix-fixit-at-point
    "RB" 'rtags-show-rtags-buffer
    "RI" 'rtags-imenu
    "RT" 'rtags-taglist
    "Rh" 'rtags-print-class-hierarchy
    "Ra" 'rtags-print-source-arguments
    ))

(provide 'keybindings)
;;; keybindings.el ends here
