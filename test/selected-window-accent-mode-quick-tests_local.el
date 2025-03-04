;; This file is a list of local tests, just evaluate each statement
;; and see what happens!, maybe in a random order if you want

;; initialisation

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1))

(require 'selected-window-accent-mode)
(selected-window-accent-mode 1)

;; check mode compatibility
(use-package visual-fill-column
  :init (visual-fill-column-mode 1))

(use-package olivetti
  :init (olivetti-mode 1))

;; general quick checks

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color "#427900")
  (selected-window-accent-mode-style 'subtle))

;; Example 1 - Default / custom color

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-custom-color "goldenrod")
  (selected-window-accent-mode-style 'default))

;; Example 2 - Tiling / custom color / custom fringe thickness

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 6)
  (selected-window-accent-custom-color "#4179b2")
  (selected-window-accent-mode-style 'tiling))

;; Example 3 - Tiling / theme highlight color

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'tiling))

;; Example 4 - Subtle / custom fringe thickness (thick)

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

;; Example 5 - Subtle / theme accent colour with darkening and desaturation

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-percentage-darken 10)
  (selected-window-accent-percentage-desaturate 100)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

;; Example 6 - Subtle / theme accent colour with lightening and saturation and tab accent

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 20)
  (selected-window-accent-percentage-darken -10)
  (selected-window-accent-percentage-desaturate -100)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))

;; Example 7 - Blending #1

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-use-blend-background t)
  (selected-window-accent-use-blend-alpha 0.2)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-custom-color "cyan4")
  (selected-window-accent-mode-style 'default))

;; Example 8 - Blending #2

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-use-pywal nil)
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-use-blend-background t)
  (selected-window-accent-use-blend-alpha 0.1)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-custom-color "orange")
  (selected-window-accent-mode-style 'subtle))
