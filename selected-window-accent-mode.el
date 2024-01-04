;; Author: James Dyer <captainflasmr@gmail.com>
;; Maintainer: Your Name <captainflasmr@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, windows
;; URL: http://https://github.com/captainflasmr/selected-window-accent-mode

;;; Commentary:

;; This package provides functions to accentuate and reset the active window
;; by changing its fringe, mode-line, and margins based on the 'highlight' face.

;;; Code:

(defvar selected-window-accent-mode nil
  "Mode variable for `selected-window-accent-mode'.")

(defun selected-window-accent (&optional custom-accent-colour)
  "Set accent colours for the selected window's fringes, mode line, and margins with optional CUSTOM-ACCENT-COLOUR."
  (interactive)
  (let* ((init-accent-colour (or custom-accent-colour (face-attribute 'highlight :background)))
         (accent-offset (if (string-greaterp init-accent-colour "#888888888888") -10 -10))
         (accent-bg-colour
          (color-desaturate-name
            (color-darken-name init-accent-colour accent-offset) 20))
         (accent-fg-colour (if (string-greaterp accent-bg-colour "#888888888888") "#000000" "#ffffff")))
    (set-face-attribute 'fringe nil :background accent-bg-colour)
    (set-face-attribute 'mode-line-active nil :background accent-bg-colour :foreground accent-fg-colour)
    (walk-windows
     (lambda (window)
       (if (eq window (selected-window))
           (progn
             (set-window-margins window 1 0)
             (with-selected-window window
               (if (eq visual-fill-column-mode t)
                   (visual-fill-column-mode t)))
             (set-window-fringes window 10 10 t nil))
         (progn
           (set-window-margins window 2 0)
           (with-selected-window window
             (if (eq visual-fill-column-mode t)
                 (visual-fill-column-mode t)))
           (set-window-fringes window 0 0 t nil))
         ))
     nil t)))

(defun reset-window-accent ()
  "Reset the accent colours for all windows to their defaults."
  (interactive)
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'mode-line-active nil :background nil :foreground nil)
  (walk-windows
   (lambda (window)
     ;; Reset margins and fringes to default. Adjust as needed.
     (set-window-margins window 0 0)
     (set-window-fringes window 0 0 t nil))
   nil t))

(define-minor-mode selected-window-accent-mode
  "Toggle selected window accenting."
  :global t
  :lighter " SWA"
  (if selected-window-accent-mode
      (progn
        (add-hook 'window-configuration-change-hook 'selected-window-accent)
        (add-hook 'window-state-change-hook 'selected-window-accent)
        (selected-window-accent))
    (progn
      (remove-hook 'window-configuration-change-hook 'selected-window-accent)
      (remove-hook 'window-state-change-hook 'selected-window-accent)
      (reset-window-accent))))

(provide 'selected-window-accent-mode)

;;; selected-window-accent.el ends here
