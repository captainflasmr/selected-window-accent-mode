;;; selected-window-accent-mode.el --- Accent Selected Window -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1") (visual-fill-column "0.0"))
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
;; The Selected Window Accent Mode is an Emacs package designed to visually
;; distinguish the currently selected window by applying a unique accent
;; color to its fringes, mode line, header line, and margins.
;;
;;
;; 1 Quick Start
;;
;;   To use left and bottom accent based on the themes highlight colour:
;;
;;   | (use-package selected-window-accent-mode
;;   |   :config (selected-window-accent-mode 1)
;;   |   :custom
;;   |   (selected-window-accent-fringe-thickness 10)
;;   |   (selected-window-accent-custom-color nil)
;;   |   (selected-window-accent-mode-style 'subtle))
;;
;;   OR define your own colour:
;;
;;   | (use-package selected-window-accent-mode
;;   |   :config (selected-window-accent-mode 1)
;;   |   :custom
;;   |   (selected-window-accent-fringe-thickness 10)
;;   |   (selected-window-accent-custom-color "#427900")
;;   |   (selected-window-accent-mode-style 'subtle))
;;
;; 2 Alternative window highlighting packages
;;
;;   There exist a few Emacs packages that perform window highlighting but
;;   that don't quite provide the feature set of selected-window-accent.
;;
;;   selected-window-accent focusses more on clearly but non-intrusively
;;   highlighting the currently selected/focussed window by highlighting
;;   aspects of the window border without having to modify the appearance
;;   of non-selected windows, hence more akin to a tiling window manager.
;;
;; 2.1 dimmer
;;
;;   "This package provides a minor mode that indicates which buffer is
;;   currently active by dimming the faces in the other buffers."
;;
;;   This is the closest in functionality to selected-window-accent, the
;;   difference being that dimmer dims non selected windows rather than
;;   accent the selected window.
;;
;;   dimmer can be used in conjunction and will complement
;;   selected-window-accent to further enhance the emphasizing of the
;;   selected window.
;;
;; 2.2 hiwin
;;
;;   "This package provides a minor-mode to change the background colour of
;;   the non active window."
;;
;;   It uses overlays to highlight non active windows, so is similar to
;;   dimmer but is less subtle in its highlighting mechanism and hasn't
;;   been updated in excess of 10 years.
;;
;; 2.3 color-theme-buffer-local
;;
;;   "This package lets you set a color-theme on a per-buffer basis."
;;
;;   Unlike dimmer and hiwin this package isn't related to the concept of a
;;   selected window but more of defining different themes for different
;;   windows to distinguish them.
;;
;; 2.4 solaire-mode
;;
;;   "This package is designed to visually distinguish "real" buffers
;;   (i.e. file-visiting code buffers where you do most of your work) from
;;   "unreal" buffers (like popups, sidebars, log buffers, terminals, etc)
;;   by giving the latter a slightly different -- often darker --
;;   background"
;;
;;   Unlike dimmer and hiwin this package isn't related to the concept of a
;;   selected window but more of distinguishing between collections of IDE
;;   like elements within Emacs.
;;
(require 'color)
(require 'visual-fill-column)

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

(defun selected-window-accent--window-update (window is-selected)
  "Update fringes and margins for the given WINDOW.
IS-SELECTED defines if the current window is being processed"
  (pcase selected-window-accent-mode-style
    ('tiling
      (setq header-line-format '(""))
      (set-face-attribute 'header-line nil :height (* 6 selected-window-accent-fringe-thickness))
      (set-face-attribute 'mode-line-active nil :height (* 8 selected-window-accent-fringe-thickness))
      (set-window-margins window (if is-selected 1 2) 0)
      (when (featurep 'visual-fill-column)
        (with-selected-window window
          (when (eq visual-fill-column-mode t) (visual-fill-column-mode t))))
      (set-window-fringes window
        selected-window-accent-fringe-thickness
        selected-window-accent-fringe-thickness 0 t))
    ('subtle
      (setq header-line-format 'nil)
      (set-window-margins window (if is-selected 1 2) 0)
      (when (featurep 'visual-fill-column)
        (with-selected-window window
          (when (eq visual-fill-column-mode t) (visual-fill-column-mode t))))
      (set-window-fringes window
        selected-window-accent-fringe-thickness 0 0 t))
    ('default
      (set-window-margins window 0 0)
      (when (featurep 'visual-fill-column)
        (with-selected-window window
          (when (eq visual-fill-column-mode t) (visual-fill-column-mode t))))
      (set-window-fringes window 0 0 0 t))))

(defun selected-window-accent--color-name-to-hex (color-name)
  "Convert COLOR-NAME to its hexadecimal representation."
  (let ((rgb (color-name-to-rgb color-name)))
    (when rgb
      (apply #'format "#%02x%02x%02x"
        (mapcar (lambda (x) (round (* x 255))) rgb)))))

(defun selected-window-accent--more-than-one-window-p ()
  "Return t if the current frame has more than one window."
  (> (length (window-list)) 1))

(defun selected-window-accent (&optional custom-accent-color)
  "Set accent colors for the selected window fringes, mode line, and margins.
With optional CUSTOM-ACCENT-COLOR, explicitly defined color"
  (interactive "P")
  (when custom-accent-color
    (setq selected-window-accent-custom-color (read-color "Enter custom accent color: ")))

  (let* ((accent-bg-color)
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

    (setq accent-fg-color (if (string-greaterp accent-bg-color "#888888") "#000000" "#ffffff"))

    (set-face-attribute 'fringe nil :background accent-bg-color :foreground accent-bg-color)

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
          (selected-window-accent--window-update window is-selected)
          (with-selected-window window
            (when (not is-selected)
              (setq header-line-format 'nil)
              (set-window-fringes window 0 0 0 t))))
        nil t))))

(defun selected-window-accent--reset-window-accent ()
  "Reset the accent colors for all windows to their defaults."
  (interactive)
  (set-face-attribute 'fringe nil :background nil :foreground nil)
  (set-face-attribute 'mode-line-active nil :background nil :foreground nil)
  (set-face-attribute 'header-line nil :background nil :foreground nil)
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

(defun selected-window-accent--switch-selected-window-accent-style (style)
  "Switch the selected window accent style to STYLE and apply it."
  (interactive
    (list (intern (completing-read "Choose accent style: " '(default tiling subtle)))))
  (customize-set-variable 'selected-window-accent-mode-style style)
  (selected-window-accent))

(provide 'selected-window-accent-mode)

;;; selected-window-accent-mode.el ends here
