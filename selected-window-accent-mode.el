;; Author: James Dyer <captainflasmr@gmail.com>
;; Maintainer: Your Name <captainflasmr@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, windows
;; URL: http://https://github.com/captainflasmr/selected-window-accent-mode

;;; Commentary:

;; 1 Summary
;; =========

;;   <file:selected-window-accent-mode-00.jpg>

;;   The Selected Window Accent Mode is an Emacs package designed to
;;   visually distinguish the currently selected window by applying a
;;   unique accent color to its fringes, mode line, header line, and
;;   margins.


;; 2 Quick Start (emacs 29)
;; ========================

;;   Add the following to the emacs init for a tiling window manager feel:

;;   ,----
;;   | (use-package selected-window-accent-mode
;;   |   :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode")
;;   |   :custom
;;   |   (selected-window-accent-fringe-thickness 10)
;;   |   (selected-window-accent-custom-color "#916941")
;;   |   (selected-window-accent-mode-style 'tiling))
;;   |
;;   | (selected-window-accent-mode 1)
;;   `----


;; 3 Installation
;; ==============

;; 3.1 use-package (emacs 29)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Put the following into your emacs init file:

;;   ,----
;;   | (use-package selected-window-accent-mode
;;   |   :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode"))
;;   `----


;; 3.2 use-package (MELPA)
;; ~~~~~~~~~~~~~~~~~~~~~~~

;;   - TODO (see roadmap below)


;; 3.3 from source
;; ~~~~~~~~~~~~~~~

;;   Download the `.el` file and place it in your Emacs `load-path`.

;;   Then either manually load it or add it to your configuration to be
;;   loaded at startup.

;;   ,----
;;   | (require 'selected-window-accent-mode)
;;   `----


;; 4 Usage
;; =======

;;   Interactively Toggle the mode on and off `M-x
;;   selected-window-accent-mode'

;;   Interactively change the current style `M-x
;;   switch-selected-window-accent-style' which will present a
;;   `completing-read' selection in the minibuffer

;;   The styles that are currently supported :

;;   - default
;;   - tiling
;;   - subtle

;;   see *roadmap* below for a description.

;;   Typically I have bound these two interactive functions to a new keymap
;;   where I keep all my emacs visual change functions.

;;   ,----
;;   | (defvar my-win-keymap (make-sparse-keymap))
;;   | (global-set-key (kbd "M-o") my-win-keymap)
;;   | (define-key my-win-keymap (kbd "a") 'selected-window-accent-mode)
;;   | (define-key my-win-keymap (kbd "y") 'switch-selected-window-accent-style)
;;   `----


;; 5 Examples
;; ==========

;; 5.1 Example 1 - Default / custom color
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   <file:selected-window-accent-mode-01.jpg>

;;   To enable the accent mode automatically upon starting Emacs, add the
;;   following line to your `.emacs` or `init.el` file:

;;   ,----
;;   | (use-package selected-window-accent-mode
;;   |   :load-path "~/repos/selected-window-accent-mode"
;;   |   :custom
;;   |   (selected-window-accent-custom-color "goldenrod")
;;   |   (selected-window-accent-mode-style 'default))
;;   |
;;   | (selected-window-accent-mode 1)
;;   `----

;;   This will accent the modeline only for the selected window with the
;;   `goldenrod' color.


;; 5.2 Example 2 - Tiling / custom color / custom fringe thickness
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   <file:selected-window-accent-mode-02.jpg>

;;   ,----
;;   | (setq selected-window-accent-fringe-thickness 6)
;;   | (setq selected-window-accent-custom-color "#4179b2")
;;   | (setq selected-window-accent-mode-style 'tiling)
;;   |
;;   | (selected-window-accent-mode 1)
;;   `----

;;   This will accent the full outline of the window with the color #4179b2
;;   more akin to a tiling window manager.


;; 5.3 Example 3 - Tiling / theme highlight color
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   <file:selected-window-accent-mode-03.jpg>

;;   ,----
;;   | (setq selected-window-accent-custom-color nil)
;;   | (setq selected-window-accent-mode-style 'tiling)
;;   |
;;   | (selected-window-accent-mode 1)
;;   `----

;;   This will accent the full outline of the window with the `highlight'
;;   color taken from the current theme.


;; 5.4 Example 4 - Subtle / custom fringe thickness (thick)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   <file:selected-window-accent-mode-04.jpg>

;;   ,----
;;   | (setq selected-window-accent-fringe-thickness 40)
;;   | (setq selected-window-accent-custom-color nil)
;;   | (setq selected-window-accent-mode-style 'subtle)
;;   |
;;   | (selected-window-accent-mode 1)
;;   `----

;;   This will accent the modeline and just the left fringe and in this
;;   case be quite a pronounced thick accent.


;; 6 Customization
;; ===============

;;   Can be done through the customization interface:

;;   *Selected Window Accent Group group:*

;;   Customization group for the selected-window-accent package.

;;   *Selected Window Accent Custom Color*

;;   Custom accent color for the selected window. Set this variable to
;;   change the accent color.

;;   - `None' - color will be using the current `highlight' face
;;   - `Custom Color' - input color name or Hex

;;   *Selected Window Accent Fringe Thickness:* Integer:

;;   The thickness of the fringes in pixels.

;;   *Selected Window Accent Mode*: `Boolean': Toggle

;;   Non-nil if Selected-Window-Accent mode is enabled

;;   *Selected Window Accent Mode Style*

;;   Current style for accenting the selected window.

;;   - `default' - just modeline accent
;;   - `tiling' - window border accent
;;   - `subtle' - left and modeline accent


;; 7 Minor Mode
;; ============

;;   The `selected-window-accent-mode' is a global minor mode that you can
;;   toggle to enable or disable the accenting of the selected window.

;;   When enabled, it distinguishes the selected window with a special
;;   accent color.


;; 8 Hooks
;; =======

;;   Two hooks are used to automatically update the window accents when the
;;   window configuration or state changes:

;;   - window-configuration-change-hook
;;   - window-state-change-hook

;;   These are added when the `selected-window-accent-mode' is enabled and
;;   removed when disabled.


;; 9 BUGS
;; ======

;;   The current version is pretty rough and probably definitely pre-alpha.

;;   ---

;;   Fix these to get to a tagged Version 0.1.

;;   In order of priority

;;   - *TODO* header-line not shown on window split.
;;   - *TODO* adjust the not selected-window margin to avoid little window
;;      navigation. disruption, hence translating a fringe pixel width to a
;;      number of margin characters, not quite sure how I am going to do
;;      this yet.
;;   - *TODO* Incorporate `mode-line-active' and `mode-line-inactive'
;;      somehow as this would make more sense especially in the 'default
;;      mode.
;;   - *TODO* excess selected-window disruption in header-line.
;;   - *WATCHING* careful with removing header-line on all windows, for
;;      example magit commit window and probably some others may need to
;;      add some logic depending on mode.


;; 10 roadmap
;; ==========

;; 10.1 *DOING* add screenshot examples
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; 10.2 add to MELPA
;; ~~~~~~~~~~~~~~~~~


;; 10.3 define more custom variables:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   - accent color saturation adjustment
;;   - accent color darken adjustment
;;   - accent color hue adjustment


;; 10.4 define which theme face attribute to use as the main accent color
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Currently the default is to use the `highlight' face


;; 10.5 *DOING* implement accent styles
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   - *DONE* `default' - /bottom/ - full height modeline
;;   - *DOING* `tiling' - /top/right/bottom/left/ - typically a squished
;;      modeline and header line to a general accent thickness to provide a
;;      typical tiling window manager focussed outline experience
;;   - *DOING* `subtle' - /left/bottom/
;;   - *TODO* `full' - /top/right/bottom/left/ - full height modeline
;;      (currently implemented as `tiling' but will be moved when tiling is
;;      more "tiling")

;;; Code:

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
Possible values are tiling default."
  :type '(choice
           (const :tag "Default Style" default)
           (const :tag "Tiling Style" tiling)
           (const :tag "Subtle Style" subtle))
  :group 'selected-window-accent-group)

(defun selected-window-accent (&optional custom-accent-colour)
  "Set accent colours for the selected window fringes, mode line, and margins"
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
              (set-window-fringes window selected-window-accent-fringe-thickness
                selected-window-accent-fringe-thickness t nil)
              )
            ('subtle
              (set-window-margins window 1 0)
              (with-selected-window window
                (setq header-line-format nil)
                (if (eq visual-fill-column-mode t)
                  (visual-fill-column-mode t)))
              (set-window-fringes window selected-window-accent-fringe-thickness
                0 t nil)
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
            ('subtle
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
    (list (intern (completing-read "Choose accent style: " '(default tiling subtle)))))
  (custom-set-variables '(selected-window-accent-mode-style style))
  (selected-window-accent))

(provide 'selected-window-accent-mode)

;;; selected-window-accent.el ends here
