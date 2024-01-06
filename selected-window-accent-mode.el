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

(require 'color)
(require 'visual-fill-column)

(defgroup selected-window-accent-group nil
  "Customization group for the selected-window-accent package."
  :group 'convenience)

(defcustom selected-window-accent-custom-color nil
  "Custom accent color for the selected window. Set this variable to change the accent color."
  :type '(choice (const :tag "None" nil)
                 (color :tag "Custom Color"))
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-mode nil
  "Mode variable for `selected-window-accent-mode'."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-mode-style 'default
  "Current style for accenting the selected window.
Possible values are 'tiling, 'default."
  :type '(choice (const :tag "Default Style" default)
                 (const :tag "Tiling Style" tiling))
  :group 'selected-window-accent-group)

(defun selected-window-accent (&optional custom-accent-colour)
  "Set accent colours for the selected window's fringes, mode line, and margins with optional CUSTOM-ACCENT-COLOUR."
  (interactive "P")

  (when custom-accent-colour
    (setq selected-window-accent-custom-color (read-color "Enter custom accent color: ")))

  (let* ((init-accent-colour (or selected-window-accent-custom-color
                               (face-attribute 'highlight :background)))
          (accent-bg-colour
            (color-desaturate-name
              (color-darken-name init-accent-colour 10) 10)) ; Adjust desaturation and darkening as needed
          (accent-fg-colour (if (string-greaterp accent-bg-colour "#888888888888") "#000000" "#ffffff")))

    ;; set up the colour
    (set-face-attribute 'fringe nil :background accent-bg-colour)
    (set-face-attribute 'mode-line-active nil :background accent-bg-colour)
    (set-face-attribute 'header-line nil :background accent-bg-colour)

    (walk-windows
      (lambda (window)
        (if (eq window (selected-window))
          (pcase selected-window-accent-mode-style
            ('tiling
              (set-window-margins window 1 0)
              (with-selected-window window
                (setq header-line-format '(""))
                (if (eq visual-fill-column-mode t)
                  (visual-fill-column-mode t)))
              (set-window-fringes window 6 6 t nil)
              )
            ('default
              (with-selected-window window
                (setq header-line-format nil)
                (if (eq visual-fill-column-mode t)
                  (visual-fill-column-mode t)))
              (set-window-fringes window 0 0 t nil)
              )
            )
          (pcase selected-window-accent-mode-style
            ('tiling
              (set-window-margins window 2 0)
              (with-selected-window window
                (setq header-line-format nil)
                (if (eq visual-fill-column-mode t)
                  (visual-fill-column-mode t)))
              (set-window-fringes window 0 0 t nil)
              )
            ('default
              (with-selected-window window
                (setq header-line-format nil)
                (if (eq visual-fill-column-mode t)
                  (visual-fill-column-mode t)))
              (set-window-fringes window 0 0 t nil)
              )
            )
          )
        )
      nil t)
    )
  )

(defun reset-window-accent ()
  "Reset the accent colours for all windows to their defaults."
  (interactive)
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'mode-line-active nil :background nil :foreground nil)
  (set-face-attribute 'header-line nil :background nil :foreground nil)
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

(defun switch-selected-window-accent-style (style)
  "Switch the selected window accent style to STYLE and apply it."
  (interactive
   (list (intern (completing-read "Choose accent style: " '(default tiling)))))
  (custom-set-variables '(selected-window-accent-mode-style style))
  (selected-window-accent))

(provide 'selected-window-accent-mode)

;;; selected-window-accent.el ends here
