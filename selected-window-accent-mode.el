;;; selected-window-accent-mode.el --- Accent Selected Window -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1")(transient "0.1.0"))
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

(defcustom selected-window-accent-fringe-thickness 6
  "Thickness of the accent fringes in pixels."
  :type 'integer
  :group 'selected-window-accent)

(defcustom selected-window-accent-custom-color nil
  "Custom accent color for the selected window.
When nil, uses the current theme's highlight color."
  :type '(choice (const :tag "None" nil)
                 (color :tag "Custom Color"))
  :group 'selected-window-accent)

(defcustom selected-window-accent-mode-style 'default
  "Style for accenting the selected window.
- `default': Standard Emacs appearance
- `tiling': Thicker fringes and mode line
- `subtle': Minimal visual change with accent color"
  :type '(choice (const :tag "Default Style" default)
                 (const :tag "Tiling Style" tiling)
                 (const :tag "Subtle Style" subtle))
  :group 'selected-window-accent)

(defcustom selected-window-accent-percentage-darken 20
  "Percentage to darken the accent color."
  :type 'integer
  :group 'selected-window-accent)

(defcustom selected-window-accent-percentage-desaturate 20
  "Percentage to desaturate the accent color."
  :type 'integer
  :group 'selected-window-accent)

(defcustom selected-window-accent-tab-accent nil
  "When non-nil, accent the selected tab in the tab-bar."
  :type 'boolean
  :group 'selected-window-accent)

(defcustom selected-window-accent-smart-borders nil
  "When non-nil, don't accent single-window frames."
  :type 'boolean
  :group 'selected-window-accent)

(defun selected-window-accent--pixels-to-chars (pixels)
  "Convert PIXELS to an approximate character width."
  (round (/ pixels (frame-char-width))))

(defun selected-window-accent--color-name-to-hex (color-name)
  "Convert COLOR-NAME to its hexadecimal representation."
  (let ((rgb (color-name-to-rgb color-name)))
    (when rgb
      (apply #'format "#%02x%02x%02x"
             (mapcar (lambda (x) (round (* x 255))) rgb)))))

(defun selected-window-accent--more-than-one-window-p ()
  "Return t if the current frame has more than one window."
  (> (length (window-list)) 1))

(defun selected-window-accent--determine-foreground (bg-color)
  "Determine appropriate foreground color based on BG-COLOR brightness."
  (if (string-greaterp bg-color "#888888") "#000000" "#ffffff"))

(defun selected-window-accent-sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive))) ;; Fallback to mode-line-inactive
    (custom-set-faces
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))

(defun selected-window-accent (&optional custom-accent-color)
  "Set accent colors for the selected window.
With optional CUSTOM-ACCENT-COLOR, use the provided color."
  (interactive "P")
  (when custom-accent-color
    (setq selected-window-accent-custom-color (read-color "Enter custom accent color: ")))
  
  (let* ((background-color (selected-window-accent--color-name-to-hex 
                           (face-attribute 'default :background)))
         (accent-bg-color (if selected-window-accent-custom-color
                             (selected-window-accent--color-name-to-hex 
                              selected-window-accent-custom-color)
                           (let ((base-color (selected-window-accent--color-name-to-hex 
                                             (face-attribute 'highlight :background))))
                             (setq base-color (color-darken-name base-color 
                                                                selected-window-accent-percentage-darken))
                             (color-desaturate-name base-color 
                                                   selected-window-accent-percentage-desaturate))))
         (accent-fg-color (selected-window-accent--determine-foreground accent-bg-color))
         (smart-borders-active (and selected-window-accent-smart-borders
                                   (not (selected-window-accent--more-than-one-window-p))))
         (fringe-chars (selected-window-accent--pixels-to-chars 
                        selected-window-accent-fringe-thickness)))
    
    ;; Configure faces based on style
    (pcase selected-window-accent-mode-style
      ('tiling
       (set-face-attribute 'fringe nil :background accent-bg-color :foreground accent-bg-color)
       (set-face-attribute 'mode-line-active nil 
                           :height (* 8 selected-window-accent-fringe-thickness)))
      ('subtle
       (set-face-attribute 'fringe nil :background accent-bg-color :foreground accent-bg-color)
       (set-face-attribute 'mode-line-active nil :height 'unspecified))
      
      ('default
       (set-face-attribute 'fringe nil :background background-color :foreground background-color)
       (set-face-attribute 'mode-line-active nil :height 'unspecified)))
    
    ;; Set mode-line colors
    (if smart-borders-active
        (set-face-attribute 'mode-line-active nil :background 'unspecified :foreground 'unspecified)
      (set-face-attribute 'mode-line-active nil :background accent-bg-color :foreground accent-fg-color))
    
    ;; Set tab colors if requested
    (if selected-window-accent-tab-accent
        (set-face-attribute 'tab-bar-tab nil :background accent-bg-color :foreground accent-fg-color)
      (set-face-attribute 'tab-bar-tab nil :background 'unspecified :foreground 'unspecified))
    
    ;; Configure windows
    (walk-windows
     (lambda (window)
       (let ((is-selected (and (not smart-borders-active) (eq window (selected-window)))))
         (with-selected-window window
           (if is-selected
               (progn
                 ;; Selected window: use fringes, no margins
                 (pcase selected-window-accent-mode-style
                   ('tiling
                    (set-window-margins window 0 0)
                    (set-window-fringes window
                                       selected-window-accent-fringe-thickness
                                       selected-window-accent-fringe-thickness 0 t))
                   ('subtle
                    (set-window-margins window 0 0)
                    (set-window-fringes window
                                       selected-window-accent-fringe-thickness 0 0 t))))
             ;; Non-selected window: use margins to compensate for missing fringes
             (progn
               (pcase selected-window-accent-mode-style
                 ('tiling
                  (set-window-fringes window 0 0 0 t)
                  (set-window-margins window fringe-chars fringe-chars))
                 ('subtle
                  (set-window-fringes window 0 0 0 t)
                  (set-window-margins window fringe-chars 0))))))))
     nil t)))

(defun selected-window-accent--reset ()
  "Reset window accents to defaults."
  (set-face-attribute 'fringe nil :background 'unspecified :foreground 'unspecified)
  (set-face-attribute 'mode-line-active nil :background 'unspecified 
                      :foreground 'unspecified :height 'unspecified)
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
      (selected-window-accent--reset))))

(defun selected-window-accent-switch-style (style)
  "Switch the selected window accent STYLE and apply it."
  (interactive
   (list (intern (completing-read "Choose accent style: " '(default tiling subtle)))))
  (customize-set-variable 'selected-window-accent-mode-style style)
  (selected-window-accent))

(defun selected-window-accent-switch-color ()
  "Switch the selected window accent color."
  (interactive)
  (selected-window-accent t))

(defun selected-window-accent-toggle-tab-accent ()
  "Toggle tab bar accenting."
  (interactive)
  (setq selected-window-accent-tab-accent
        (not selected-window-accent-tab-accent))
  (selected-window-accent))

(defun selected-window-accent-toggle-smart-borders ()
  "Toggle smart borders behavior."
  (interactive)
  (setq selected-window-accent-smart-borders
        (not selected-window-accent-smart-borders))
  (selected-window-accent))

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
