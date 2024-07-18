;;; selected-window-accent-mode.el --- Accent Selected Window -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience
;; URL: https://github.com/captainflasmr/selected-window-accent-mode
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The Selected Window Accent Mode is an Emacs package designed to
;; visually distinguish the currently selected window by applying a
;; unique accent color to its fringes, mode line, header line, and
;; margins.
;;
;;; Quick Start
;;
;; To use left and bottom accent based on the themes highlight colour:
;;
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'subtle))
;;
;; OR define your own colour:
;;
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color "#427900")
;;   (selected-window-accent-mode-style 'subtle))
;;
;; OR subtle / theme accent colour with lightening and saturation and
;; tab accent
;;
;; The takes the default highlight colour from the current theme but
;; applies lightening and saturation along with the same colour tab
;; accent.
;;
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 20)
;;   (selected-window-accent-percentage-darken -10)
;;   (selected-window-accent-percentage-desaturate -100)
;;   (selected-window-accent-tab-accent t)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'subtle))
;;
;;; Usage
;;
;; Interactively Toggle the mode on and off =M-x selected-window-accent-mode=
;;
;; A transient map is available (Emacs 28.1+):
;;
;; (global-set-key (kbd "C-c w") 'selected-window-accent-transient)
;;
;; which will bring up a transient menu
;;
;; The styles that are currently supported :
;;
;; - default
;; - tiling
;; - subtle
;;;
(require 'color)

;;; Code:

(defgroup selected-window-accent-group nil
  "Customization group for the `selected-window-accent' package."
  :group 'convenience)

(defcustom selected-window-accent-fringe-thickness 6
  "The thickness of the fringes in pixels.

This thickness is used when the `selected-window-accent-mode-style' is
either tiling or subtle."
  :type 'integer
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-custom-color nil
  "Custom accent color for the selected window.

When set to a color, this color will override the default highlight face
background color as the accent color for the selected window.  Setting
this to nil disables the custom color, reverting to the default
behavior."
  :type '(choice (const :tag "None" nil)
                 (color :tag "Custom Color"))
  :group 'selected-window-accent-group)

(defvar selected-window-accent-fg-color "#ffffff"
  "Custom foreground accent color for the selected window.")

(defcustom selected-window-accent-mode nil
  "Mode variable for `selected-window-accent-mode'.

When non-nil, the `selected-window-accent-mode` is active, accenting the
selected window according to the style defined in
`selected-window-accent-mode-style`."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-mode-style 'default
  "Current style for accenting the selected window.

The style determines how the selected window is visually distinguished
from unselected ones.

- `default': No special styling, uses the default Emacs appearance.

- `tiling': Accentuates the fringes and mode line of the selected window
  with a thicker appearance, based on
  `selected-window-accent-fringe-thickness`.

- `subtle': Adds a subtle accent to the selected window with minimal
  visual change."
  :type '(choice (const :tag "Default Style" default)
                 (const :tag "Tiling Style" tiling)
                 (const :tag "Subtle Style" subtle))
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-percentage-darken 20
  "The percentage the highlight accent is darkened.

This percentage of darkening used when the
`selected-window-accent-custom-color' is set to nil and hence the color
is chosen from the current theme."
  :type 'integer
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-percentage-desaturate 20
  "The percentage the highlight accent is saturated.

This percentage of desaturation used when the
`selected-window-accent-custom-color' is set to nil and hence the color
is chosen from the current theme."
  :type 'integer
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-tab-accent nil
  "When non-nil, the `selected-window-accent-tab-accent` is active.
Accenting the selected selected tab in the tab-bar."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-smart-borders nil
  "When non-nil, the `selected-window-accent-smart-borders` is active.
Doesn't accent when a frame contains only a single window."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent-foreground-adjust-factor 0.1
  "Adjustment factor for incrementing or decrementing foreground color brightness."
  :type 'float
  :group 'selected-window-accent-group)

(defcustom selected-window-accent--use-complementary-color nil
  "Toggle complementary color for foreground based on background color."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent--foreground-invert-state nil
  "Toggle inverted color for foreground based on current foreground color."
  :type 'boolean
  :group 'selected-window-accent-group)

(defcustom selected-window-accent--foreground-offset 0
  "The percentage the foreground is modified."
  :type 'integer
  :group 'selected-window-accent-group)

(defun selected-window-accent--window-update (window)
  "Update fringes and margins for the given WINDOW.
IS-SELECTED defines if the current window is being processed"
  (pcase selected-window-accent-mode-style
    ('tiling
      (setq header-line-format '(""))
      (set-face-attribute 'header-line nil :height (* 6 selected-window-accent-fringe-thickness))
      (set-face-attribute 'mode-line-active nil :height (* 8 selected-window-accent-fringe-thickness))
      (set-window-fringes window
        selected-window-accent-fringe-thickness
        selected-window-accent-fringe-thickness 0 t))
    ('subtle
      (set-face-attribute 'mode-line-active nil :height 'unspecified)
      (setq header-line-format 'nil)
      (set-window-fringes window
        selected-window-accent-fringe-thickness 0 0 t))
    ('default
      (set-face-attribute 'mode-line-active nil :height 'unspecified))))

(defun selected-window-accent--color-name-to-hex (color-name)
  "Convert COLOR-NAME to its hexadecimal representation."
  (let ((rgb (color-name-to-rgb color-name)))
    (when rgb
      (apply #'format "#%02x%02x%02x"
        (mapcar (lambda (x) (round (* x 255))) rgb)))))

(defun selected-window-accent--more-than-one-window-p ()
  "Return t if the current frame has more than one window."
  (> (length (window-list)) 1))

(defun selected-window-accent--increment-color-brightness (hex-color factor)
  "Increment the brightness of HEX-COLOR by FACTOR."
  (let* ((rgb (color-name-to-rgb hex-color))
         (new-rgb (mapcar (lambda (x) (min 1.0 (+ x factor))) rgb)))
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (x) (round (* x 255))) new-rgb))))

(defun selected-window-accent--decrement-color-brightness (hex-color factor)
  "Decrement the brightness of HEX-COLOR by FACTOR."
  (let* ((rgb (color-name-to-rgb hex-color))
         (new-rgb (mapcar (lambda (x) (max 0.0 (- x factor))) rgb)))
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (x) (round (* x 255))) new-rgb))))

(defun selected-window-accent--invert-color (hex-color)
  "Invert HEX-COLOR to its opposite value."
  (let* ((rgb (color-name-to-rgb hex-color))
         (inverted-rgb (mapcar (lambda (x) (- 1 x)) rgb)))
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (x) (round (* x 255))) inverted-rgb))))

(defun selected-window-accent-flip-foreground-color ()
  "Flip the current foreground color to its opposite value."
  (interactive)
  (setq selected-window-accent--foreground-invert-state
    (not selected-window-accent--foreground-invert-state))
  (selected-window-accent))

(defun selected-window-accent-increment-foreground-color ()
  "Increment the foreground color brightness.  With ARG, adjust by a larger factor."
  (interactive "p")
  (let ((repeat-map (make-sparse-keymap)))
    (setq selected-window-accent--foreground-offset
      (max 0.0 (min 1.0
                    (+ selected-window-accent--foreground-offset
                       selected-window-accent-foreground-adjust-factor))))
    (selected-window-accent)
    (message "Foreground color incremented to %s" selected-window-accent--foreground-offset)
    (define-key repeat-map (kbd "+") 'selected-window-accent-increment-foreground-color)
    (define-key repeat-map (kbd "-") 'selected-window-accent-decrement-foreground-color)
    (set-transient-map repeat-map t)))

(defun selected-window-accent-decrement-foreground-color ()
  "Decrement the foreground color brightness.  With ARG, adjust by a larger factor."
  (interactive "p")
  (let ((repeat-map (make-sparse-keymap)))
    (setq selected-window-accent--foreground-offset
      (max 0.0 (min 1.0
                    (- selected-window-accent--foreground-offset
                       selected-window-accent-foreground-adjust-factor))))
    (selected-window-accent)
    (message "Foreground color decremented to %s" selected-window-accent--foreground-offset)
    (define-key repeat-map (kbd "+") 'selected-window-accent-increment-foreground-color)
    (define-key repeat-map (kbd "-") 'selected-window-accent-decrement-foreground-color)
    (set-transient-map repeat-map t)))

(defun selected-window-accent-toggle-complementary-color ()
  "Toggle between complementary color for foreground based on background color."
  (interactive)
  (setq selected-window-accent--use-complementary-color
    (not selected-window-accent--use-complementary-color))
  (selected-window-accent))

(defun selected-window-accent-toggle-tab-accent ()
  "Toggle between showing the tab accent."
  (interactive)
  (setq selected-window-accent-tab-accent
    (not selected-window-accent-tab-accent))
  (selected-window-accent))

(defun selected-window-accent-toggle-smart-borders ()
  "Toggle between smart borders."
  (interactive)
  (setq selected-window-accent-smart-borders
    (not selected-window-accent-smart-borders))
  (selected-window-accent))

(defun selected-window-accent-output-selected-window-accent-settings ()
  "Output current `selected-window-accent-mode' settings to a new buffer."
  (interactive)
  (let ((use-package-string "
(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness %d)
  (selected-window-accent-percentage-darken %d)
  (selected-window-accent-percentage-desaturate %d)
  (selected-window-accent-smart-borders %s)
  (selected-window-accent-tab-accent %s)
  (selected-window-accent-custom-color \"%s\")
  (selected-window-accent-mode-style '%s)
  (selected-window-accent-foreground-adjust-factor %.1f)
  (selected-window-accent--use-complementary-color %s)
  (selected-window-accent--foreground-invert-state %s)
  (selected-window-accent--foreground-offset %d))"))
    (with-current-buffer (get-buffer-create "*selected-window-accent-mode-settings*")
      (erase-buffer)
      (insert (format use-package-string
                      selected-window-accent-fringe-thickness
                      selected-window-accent-percentage-darken
                      selected-window-accent-percentage-desaturate
                      (if selected-window-accent-smart-borders "t" "nil")
                      (if selected-window-accent-tab-accent "t" "nil")
                      (or selected-window-accent-custom-color "nil")
                      selected-window-accent-mode-style
                      selected-window-accent-foreground-adjust-factor
                      (if selected-window-accent--use-complementary-color "t" "nil")
                      (if selected-window-accent--foreground-invert-state "t" "nil")
                      selected-window-accent--foreground-offset))
      (pop-to-buffer (current-buffer)))))

(defun selected-window-accent--set-foreground-color (bg-color)
  "Determine the foreground color based on BG-COLOR, toggle value, and store it."
  (let ((new-fg-color))
          (if selected-window-accent--use-complementary-color
            (setq new-fg-color (color-complement-hex bg-color))
            (progn
              (if (string-greaterp bg-color "#888888")
                (setq new-fg-color "#000000")
                (setq new-fg-color "#ffffff"))
              (when selected-window-accent--foreground-invert-state
                (setq new-fg-color (selected-window-accent--invert-color new-fg-color)))
              (setq new-fg-color
                (selected-window-accent--increment-color-brightness
                  new-fg-color selected-window-accent--foreground-offset))))
            (selected-window-accent--color-name-to-hex new-fg-color)))

(defun selected-window-accent (&optional custom-accent-color)
  "Set accent colors for the selected window fringes, mode line, and margins.
With optional CUSTOM-ACCENT-COLOR, explicitly defined color"
  (interactive "P")

  (when custom-accent-color
    (setq selected-window-accent-custom-color (read-color "Enter custom accent color: ")))

  (let* ((background-color (selected-window-accent--color-name-to-hex (face-attribute 'default :background)))
          (accent-bg-color)
          (accent-fg-color)
          (smart-borders-active (and selected-window-accent-smart-borders
                                  (not (selected-window-accent--more-than-one-window-p)))))
    (if (not selected-window-accent-custom-color)
      (progn
        (setq accent-bg-color
          (selected-window-accent--color-name-to-hex (face-attribute 'highlight :background)))
        (if (> selected-window-accent-percentage-darken 0)
          (setq accent-bg-color (color-darken-name accent-bg-color selected-window-accent-percentage-darken))
          (setq accent-bg-color (color-lighten-name accent-bg-color (abs selected-window-accent-percentage-darken))))
        (if (> selected-window-accent-percentage-desaturate 0)
          (setq accent-bg-color (color-desaturate-name accent-bg-color selected-window-accent-percentage-desaturate))
          (setq accent-bg-color (color-saturate-name accent-bg-color (abs selected-window-accent-percentage-desaturate)))))
      (setq accent-bg-color selected-window-accent-custom-color))

    (setq accent-fg-color (selected-window-accent--set-foreground-color accent-bg-color))

    ;; (message (concat "new color : " accent-fg-color))

    (if (eq selected-window-accent-mode-style 'default)
      (set-face-attribute 'fringe nil :background background-color :foreground background-color)
      (set-face-attribute 'fringe nil :background accent-bg-color :foreground accent-bg-color))

    (if smart-borders-active
      (set-face-attribute 'mode-line-active nil :background 'unspecified :foreground 'unspecified)
      (set-face-attribute 'mode-line-active nil :background accent-bg-color :foreground accent-fg-color))

    (set-face-attribute 'header-line nil :background accent-bg-color :foreground accent-bg-color)
    (if selected-window-accent-tab-accent
      (set-face-attribute 'tab-bar-tab nil :background accent-bg-color :foreground accent-fg-color)
      (set-face-attribute 'tab-bar-tab nil :background 'unspecified :foreground 'unspecified))

    (walk-windows
      (lambda (window)
        (let* ((is-selected (and (not smart-borders-active) (eq window (selected-window)))))
          (selected-window-accent--window-update window)
          (with-selected-window window
            (when (not is-selected)
              (setq header-line-format 'nil)
              (when (not (eq selected-window-accent-mode-style 'default))
                (set-window-fringes window 0 0 0 t)))))
        nil t))))

(defun selected-window-accent--reset-window-accent ()
  "Reset the accent colors for all windows to their defaults."
  (interactive)
  (set-face-attribute 'fringe nil :background 'unspecified :foreground 'unspecified)
  (set-face-attribute 'mode-line-active nil :background 'unspecified :foreground 'unspecified)
  (set-face-attribute 'header-line nil :background 'unspecified :foreground 'unspecified)
  (set-face-attribute 'tab-bar-tab nil :background 'unspecified :foreground 'unspecified)
  (walk-windows
    (lambda (window)
      (set-window-margins window 0 0)
      (set-window-fringes window 0 0 0 t))
    nil t))

;;;###autoload
(define-minor-mode selected-window-accent-mode
  "Toggle selected window accenting."
  :global t
  :lighter " SWA"
  (if selected-window-accent-mode
    (progn
      (add-hook 'window-configuration-change-hook #'selected-window-accent)
      (add-hook 'window-state-change-hook #'selected-window-accent)
      (selected-window-accent))
    (progn
      (remove-hook 'window-configuration-change-hook #'selected-window-accent)
      (remove-hook 'window-state-change-hook #'selected-window-accent)
      (selected-window-accent--reset-window-accent))))

(defun selected-window-accent-switch-selected-window-accent-style (style)
  "Switch the selected window accent style to STYLE and apply it."
  (interactive
    (list (intern (completing-read "Choose accent style: " '(default tiling subtle)))))
  (customize-set-variable 'selected-window-accent-mode-style style)
  (selected-window-accent))

(when (and (version<= "28.0" emacs-version) (require 'transient nil 'noerror))
  ;; Define the transient command and its bindings
  (transient-define-prefix selected-window-accent-transient ()
    "Transient for selected window accent."
    ["Selected Window Accent"
     ["Main"
      ("s" "Switch Style" selected-window-accent-switch-selected-window-accent-style)
      ("c" "Switch Color" (lambda () (interactive) (selected-window-accent t)))
      ("o" "Output Settings" selected-window-accent-output-selected-window-accent-settings)
      ("q" "Quit" transient-quit-one)]
     ["Toggle"
      ("m" "Smart Borders" selected-window-accent-toggle-smart-borders)
      ("t" "Tab" selected-window-accent-toggle-tab-accent)]
     ["Foreground"
      ("f" "Flip Value" selected-window-accent-flip-foreground-color)
      ("l" "Complementary" selected-window-accent-toggle-complementary-color)
      ("+" "Increment Value" selected-window-accent-increment-foreground-color)
      ("-" "Decrement Value" selected-window-accent-decrement-foreground-color)]]))

(provide 'selected-window-accent-mode)

;;; selected-window-accent-mode.el ends here
