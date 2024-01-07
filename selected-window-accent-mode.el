(require 'color)
(require 'visual-fill-column)

(defgroup selected-window-accent-group nil
  "Customization group for the selected-window-accent package."
  :group 'convenience)

(defcustom selected-window-accent-fringe-thickness 6
  "The thickness of the fringes in pixels."
  :type 'integer
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-custom-color nil
  "Custom accent color for the selected window."
  :type '(choice (const :tag "None" nil)
           (color :tag "Custom Color"))
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-mode nil
  "Mode variable for `selected-window-accent-mode'."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-mode-style 'default
  "Current style for accenting the selected window.
Possible values are default, tiling, or subtle."
  :type '(choice (const :tag "Default Style" default)
           (const :tag "Tiling Style" tiling)
           (const :tag "Subtle Style" subtle))
  :group 'selected-window-accent-group)

(defun window-update (window is-selected)
  "Update fringes and margins for the given WINDOW based on whether it's selected."
  (pcase selected-window-accent-mode-style
    ('tiling
      (setq header-line-format '(""))
      (set-face-attribute 'header-line nil :height (* 6 selected-window-accent-fringe-thickness))
      (set-face-attribute 'mode-line-active nil :height (* 8 selected-window-accent-fringe-thickness))
      (set-window-margins window (if is-selected 1 2) 0)
      (set-window-fringes window
        selected-window-accent-fringe-thickness
        selected-window-accent-fringe-thickness 0 t))
    ('subtle
      (setq header-line-format 'nil)
      (set-window-margins window (if is-selected 1 2) 0)
      (set-window-fringes window
        selected-window-accent-fringe-thickness 0 0 t))
    ('default
      (set-window-margins window 0 0)
      (set-window-fringes window 0 0 0 t))))

(defun color-name-to-hex (color-name)
  "Convert COLOR-NAME to its hexadecimal representation."
  (let ((rgb (color-name-to-rgb color-name)))
    (when rgb
      (apply 'format "#%02x%02x%02x"
        (mapcar (lambda (x) (round (* x 255))) rgb)))))

(defun selected-window-accent (&optional custom-accent-color)
  "Set accent colors for the selected window fringes, mode line, and margins."
  (interactive "P")

  (when custom-accent-color
    (setq selected-window-accent-custom-color (read-color "Enter custom accent color: ")))

  (let* ((init-accent-color (or selected-window-accent-custom-color
                              (color-name-to-hex (face-attribute 'highlight :background))))
          (accent-bg-color (if (string-greaterp init-accent-color "#000404")
                             (color-desaturate-name (color-darken-name init-accent-color 0) 20)
                             (color-desaturate-name (color-lighten-name init-accent-color 0) 0)))
          (accent-fg-color (if (string-greaterp accent-bg-color "#888888") "#000000" "#ffffff")))

    (set-face-attribute 'fringe nil :background accent-bg-color :foreground accent-bg-color)
    (set-face-attribute 'mode-line-active nil :background accent-bg-color :foreground accent-fg-color)
    (set-face-attribute 'header-line nil :background accent-bg-color :foreground accent-bg-color)

    (walk-windows
      (lambda (window)
        (let ((is-selected (eq window (selected-window))))
          (window-update window is-selected)

          (when (not is-selected)
            (with-selected-window window
              (setq header-line-format 'nil)
              (set-window-fringes window 0 0 0 t)
              (when visual-fill-column-mode (visual-fill-column-mode t))))
          )
        )
      nil t)))

(defun reset-window-accent ()
  "Reset the accent colors for all windows to their defaults."
  (interactive)
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'mode-line-active nil :background nil)
  (set-face-attribute 'header-line nil :background nil)
  (walk-windows
    (lambda (window)
      (set-window-margins window 0 0)
      (set-window-fringes window 0 0 0 t))
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
    (list (intern (completing-read "Choose accent style: " '(default tiling subtle)))))
  (customize-set-variable 'selected-window-accent-mode-style style)
  (selected-window-accent))

(provide 'selected-window-accent-mode)
