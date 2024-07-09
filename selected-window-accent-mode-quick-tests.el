;; This file is a list of tests, just evaluate each statement
;; and see what happens!, maybe in a random order if you want

;; initialisation

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1))

(use-package selected-window-accent-mode
  :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode")
  :config (selected-window-accent-mode 1))

(require 'selected-window-accent-mode)
(selected-window-accent-mode 1)

;; check mode compatibility

(visual-fill-column-mode)
(olivetti-mode)

;; general quick checks

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color "#427900")
  (selected-window-accent-mode-style 'subtle))

;; keymap function calls

(defvar my-win-keymap (make-sparse-keymap))
(global-set-key (kbd "M-o") my-win-keymap)
(define-key my-win-keymap (kbd "a") 'selected-window-accent-mode)
(define-key my-win-keymap (kbd "y") 'selected-window-accent--switch-selected-window-accent-style)

;; Example 1 - Default / custom color

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-custom-color "goldenrod")
  (selected-window-accent-mode-style 'default))

;; Example 2 - Tiling / custom color / custom fringe thickness

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 6)
  (selected-window-accent-custom-color "#4179b2")
  (selected-window-accent-mode-style 'tiling))

;; Example 3 - Tiling / theme highlight color

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'tiling))

;; Example 4 - Subtle / custom fringe thickness (thick)

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

;; Example 5 - Subtle / theme accent colour with darkening and desaturation

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-percentage-darken 10)
  (selected-window-accent-percentage-desaturate 100)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

;; Example 6 - Subtle / theme accent colour with lightening and saturation and tab accent

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-percentage-darken -10)
  (selected-window-accent-percentage-desaturate -100)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))
