;;; selected-window-accent-mode.el --- Accent Selected Window -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 2.3.0
;; Package-Requires: ((emacs "29.1"))
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
;;; Define your own colour
;;
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color "orange")
;;   (selected-window-accent-mode-style 'tiling)
;;   (selected-window-accent-percentage-darken 0)
;;   (selected-window-accent-percentage-desaturate 0)
;;   (selected-window-accent-tab-accent t)
;;   (selected-window-accent-smart-borders t))
;;
;;; Tweak a themes highlight colour
;;
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'tiling)
;;   (selected-window-accent-percentage-darken 20)
;;   (selected-window-accent-percentage-desaturate 20)
;;   (selected-window-accent-tab-accent t)
;;   (selected-window-accent-smart-borders t))
;;
;;   (global-set-key (kbd "C-c w") selected-window-accent-map)
;;

;;; Code:

(require 'color)

(defgroup selected-window-accent nil
  "Customization group for the `selected-window-accent-mode' package."
  :group 'convenience)

(defconst selected-window-accent--pywal-colors-file
  (expand-file-name "~/.cache/wal/colors.json")
  "Path to the Pywal generated colors JSON file.")

(defun selected-window-accent--set-and-refresh (symbol value)
  "Set SYMBOL to VALUE and refresh accents if the mode is active."
  (set-default symbol value)
  (when (bound-and-true-p selected-window-accent-mode)
    (selected-window-accent nil t)))

(defcustom selected-window-accent-fringe-thickness 6
  "Thickness of the accent fringes in pixels."
  :type 'integer
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-fringe-minimum 0
  "Minimum fringe width in pixels to preserve in default style.
In default style, fringes are normally set to zero.  Set this to a
positive value (e.g., 8) to keep fringes visible so that fringe
indicators such as line-wrapping arrows, git-gutter marks, and
flycheck icons remain usable."
  :type 'integer
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-custom-color nil
  "Custom accent color for the selected window.
When nil, uses the current theme's highlight color."
  :type '(choice (const :tag "None" nil)
                 (color :tag "Custom Color"))
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-mode-style 'default
  "Style for accenting the selected window.
- `default': Standard Emacs appearance
- `tiling': Thicker fringes and mode line
- `subtle': Minimal visual change with accent color"
  :type '(choice (const :tag "Default Style" default)
                 (const :tag "Tiling Style" tiling)
                 (const :tag "Subtle Style" subtle))
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-percentage-darken 20
  "Percentage to darken the accent color."
  :type 'integer
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-percentage-desaturate 20
  "Percentage to desaturate the accent color."
  :type 'integer
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-tab-accent nil
  "When non-nil, accent the selected tab in the tab-bar."
  :type 'boolean
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-smart-borders nil
  "When non-nil, don't accent single-window frames."
  :type 'boolean
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-use-pywal nil
  "When non-nil, use a color from Pywal generated palette."
  :type 'boolean
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defcustom selected-window-accent-pywal-color "color1"
  "Which Pywal color to use from the palette.
Common values: color0 through color15."
  :type 'string
  :set #'selected-window-accent--set-and-refresh
  :group 'selected-window-accent)

(defvar selected-window-accent--original-fringe-bg nil
  "Storage for original fringe background color.")

(defvar selected-window-accent--original-fringe-fg nil
  "Storage for original fringe foreground color.")

(defun selected-window-accent--pixels-to-chars (pixels)
  "Convert PIXELS to an approximate character width.
This is used to calculate margin widths that compensate for fringe spacing,
ensuring text alignment remains consistent across windows."
  (round (/ pixels (frame-char-width))))

(defun selected-window-accent--color-name-to-hex (color-name)
  "Convert COLOR-NAME to its hexadecimal representation.
Accepts any valid Emacs color name (e.g., `red', `blue') or color spec
and returns a hex string in the format #RRGGBB."
  (let ((rgb (color-name-to-rgb color-name)))
    (when rgb
      (apply #'format "#%02x%02x%02x"
             (mapcar (lambda (x) (round (* x 255))) rgb)))))

(defun selected-window-accent--more-than-one-window-p ()
  "Return t if the current frame has more than one window.
Used by smart-borders feature to determine whether to apply accenting."
  (not (one-window-p t)))

(defun selected-window-accent--determine-foreground (bg-color)
  "Determine appropriate foreground color based on BG-COLOR luminance.
Computes relative luminance using the ITU-R BT.601 formula and returns
black (#000000) for light backgrounds, white (#ffffff) for dark."
  (let ((rgb (color-name-to-rgb bg-color)))
    (if (and rgb (> (+ (* 0.299 (nth 0 rgb))
                       (* 0.587 (nth 1 rgb))
                       (* 0.114 (nth 2 rgb)))
                    0.5))
        "#000000"
      "#ffffff")))

(defun selected-window-accent-sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme.
Resets tab-bar, tab-bar-tab, and tab-bar-tab-inactive faces to inherit
from the default face and mode-line-inactive, effectively removing any
custom accenting applied by this mode."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive)))
    (set-face-attribute 'tab-bar nil
                        :inherit 'default
                        :background default-bg
                        :foreground default-fg)
    (set-face-attribute 'tab-bar-tab nil
                        :inherit 'default
                        :background default-fg
                        :foreground default-bg)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :inherit 'default
                        :background default-bg
                        :foreground inactive-fg)))

(defun selected-window-accent--get-pywal-color ()
  "Get a color from Pywal palette based on `selected-window-accent-pywal-color'."
  (condition-case err
      (let* ((colors-data (when (file-exists-p selected-window-accent--pywal-colors-file)
                            (with-temp-buffer
                              (insert-file-contents selected-window-accent--pywal-colors-file)
                              (goto-char (point-min))
                              (json-parse-buffer :object-type 'hash-table)))))
        (when colors-data
          (let ((colors (gethash "colors" colors-data)))
            (when colors
              (gethash selected-window-accent-pywal-color colors)))))
    (error
     (message "Error reading Pywal colors: %s" (error-message-string err))
     nil)))

(defun selected-window-accent (&optional custom-accent-color _force-update)
  "Set accent colors for the selected window.
With optional CUSTOM-ACCENT-COLOR, prompt for and use a new color.
FORCE-UPDATE argument is accepted for compatibility but currently unused."
  (interactive "P")
  (when custom-accent-color
    (setq selected-window-accent-custom-color (read-color "Enter custom accent color: ")))

  (let* ((accent-bg-color (or (cond
                                ((and selected-window-accent-use-pywal
                                      (file-exists-p selected-window-accent--pywal-colors-file))
                                 (selected-window-accent--get-pywal-color))
                                (selected-window-accent-custom-color
                                 (selected-window-accent--color-name-to-hex
                                  selected-window-accent-custom-color))
                                (t
                                 (let ((highlight-bg (face-attribute 'highlight :background nil 'default)))
                                   (when (stringp highlight-bg)
                                     (let ((base-color (selected-window-accent--color-name-to-hex highlight-bg)))
                                       (when base-color
                                         (color-desaturate-name
                                          (color-darken-name base-color
                                                             selected-window-accent-percentage-darken)
                                          selected-window-accent-percentage-desaturate)))))))
                               "#888888"))
         (accent-fg-color (selected-window-accent--determine-foreground accent-bg-color))
         (smart-borders-active (and selected-window-accent-smart-borders
                                    (not (selected-window-accent--more-than-one-window-p))))
         (fringe-chars (selected-window-accent--pixels-to-chars
                        selected-window-accent-fringe-thickness)))

    ;; Configure mode-line height based on style
    (pcase selected-window-accent-mode-style
      ('tiling
       (set-face-attribute 'mode-line-active nil
                           :height (* 8 selected-window-accent-fringe-thickness)))
      ('subtle
       (set-face-attribute 'mode-line-active nil :height 'unspecified))
      ('default
       (set-face-attribute 'mode-line-active nil :height 'unspecified)))

    ;; Set mode-line colors
    (if smart-borders-active
        (set-face-attribute 'mode-line-active nil :background 'unspecified :foreground 'unspecified)
      (set-face-attribute 'mode-line-active nil :background accent-bg-color :foreground accent-fg-color))

    ;; Set tab colors if requested
    (if selected-window-accent-tab-accent
        (set-face-attribute 'tab-bar-tab nil :background accent-bg-color :foreground accent-fg-color)
      (set-face-attribute 'tab-bar-tab nil :background 'unspecified :foreground 'unspecified))

    ;; Store original fringe colors if not already stored
    (unless selected-window-accent--original-fringe-bg
      (setq selected-window-accent--original-fringe-bg
            (face-attribute 'fringe :background nil 'default))
      (setq selected-window-accent--original-fringe-fg
            (face-attribute 'fringe :foreground nil 'default)))

    ;; Set global fringe face: accent color for tiling/subtle,
    ;; match default background for default style so indicators remain visible
    (if (eq selected-window-accent-mode-style 'default)
        (set-face-attribute 'fringe nil
                            :background (face-background 'default)
                            :foreground selected-window-accent--original-fringe-fg)
      (set-face-attribute 'fringe nil
                          :background accent-bg-color
                          :foreground accent-bg-color))

    ;; Configure windows
    (walk-windows
     (lambda (window)
       (let ((is-selected (and (not smart-borders-active) (eq window (selected-window)))))
         (if is-selected
             (progn
               ;; Selected window: use fringes with accent color, no margins
               (pcase selected-window-accent-mode-style
                 ('tiling
                  (set-window-margins window 0 0)
                  (set-window-fringes window
                                     selected-window-accent-fringe-thickness
                                     selected-window-accent-fringe-thickness 0 nil))
                 ('subtle
                  (set-window-margins window 0 0)
                  (set-window-fringes window
                                     selected-window-accent-fringe-thickness 0 0 nil))
                 ('default
                  (set-window-margins window 0 0)
                  (set-window-fringes window
                                     selected-window-accent-fringe-minimum
                                     selected-window-accent-fringe-minimum 0 nil))))
           ;; Non-selected window: set fringes to 0, use margins to compensate
           (progn
             (pcase selected-window-accent-mode-style
               ('tiling
                (set-window-fringes window 0 0 0 nil)
                (set-window-margins window fringe-chars fringe-chars))
               ('subtle
                (set-window-fringes window 0 0 0 nil)
                (set-window-margins window fringe-chars 0))
               ('default
                (set-window-fringes window
                                   selected-window-accent-fringe-minimum
                                   selected-window-accent-fringe-minimum 0 nil)
                (set-window-margins window 0 0)))))))
     nil t)))

(defun selected-window-accent--reset ()
  "Reset window accents to defaults.
Removes all face customizations and restores default fringe and margin
settings for all windows. Called when the mode is disabled."
  ;; Restore original fringe colors
  (when selected-window-accent--original-fringe-bg
    (set-face-attribute 'fringe nil
                        :background selected-window-accent--original-fringe-bg
                        :foreground selected-window-accent--original-fringe-fg)
    (setq selected-window-accent--original-fringe-bg nil
          selected-window-accent--original-fringe-fg nil))

  (set-face-attribute 'mode-line-active nil :background 'unspecified
                      :foreground 'unspecified :height 'unspecified)
  (set-face-attribute 'tab-bar-tab nil :background 'unspecified :foreground 'unspecified)

  (walk-windows
   (lambda (window)
     (set-window-margins window 0 0)
     (set-window-fringes window 0 0 0 nil))
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
      (selected-window-accent--reset))))

(defun selected-window-accent-switch-style (style)
  "Switch the selected window accent STYLE and apply it.
Prompts for a style choice (default, tiling, or subtle) and immediately
applies it to all windows. Displays a confirmation message."
  (interactive
   (list (intern (completing-read "Choose accent style: " '(default tiling subtle)))))
  (customize-set-variable 'selected-window-accent-mode-style style)
  (message "Switched to %s accent style" style)
  (selected-window-accent nil t))

(defun selected-window-accent-switch-color ()
  "Switch the selected window accent color.
Prompts for a new color using the standard Emacs color picker.
Accepts color names (e.g., `red', `blue') or hex values (e.g., `#ff0000')."
  (interactive)
  (selected-window-accent t))

(defun selected-window-accent-toggle-tab-accent ()
  "Toggle tab bar accenting.
When enabled, the active tab in the tab-bar will use the same accent
color as the selected window. Displays a confirmation message."
  (interactive)
  (setq selected-window-accent-tab-accent
        (not selected-window-accent-tab-accent))
  (message "Tab bar accenting %s"
           (if selected-window-accent-tab-accent "enabled" "disabled"))
  (selected-window-accent nil t))

(defun selected-window-accent-toggle-smart-borders ()
  "Toggle smart borders behavior.
When enabled, accenting is disabled when only one window exists in a frame.
This provides a cleaner look when working with single-window layouts.
Displays a confirmation message."
  (interactive)
  (setq selected-window-accent-smart-borders
        (not selected-window-accent-smart-borders))
  (message "Smart borders %s"
           (if selected-window-accent-smart-borders "enabled" "disabled"))
  (selected-window-accent nil t))

;; Simple command map for key bindings
(defvar selected-window-accent-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") 'selected-window-accent-switch-style)
    (define-key map (kbd "RET") 'selected-window-accent-switch-color)
    (define-key map (kbd "t") 'selected-window-accent-toggle-tab-accent)
    (define-key map (kbd "l") 'selected-window-accent-sync-tab-bar-to-theme)
    (define-key map (kbd "b") 'selected-window-accent-toggle-smart-borders)
    map)
  "Keymap for selected-window-accent commands.")

(provide 'selected-window-accent-mode)

;;; selected-window-accent-mode.el ends here
