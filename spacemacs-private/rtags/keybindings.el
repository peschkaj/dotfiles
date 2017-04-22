
;;;
;;; Adapted from comment:
;;; https://github.com/syl20bnr/spacemacs/issues/2327#issuecomment-153283156
;;; by user
;;; https://github.com/autosquid
;;;
(defun rtags-major-mode-keybindings (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
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
    )
  )
