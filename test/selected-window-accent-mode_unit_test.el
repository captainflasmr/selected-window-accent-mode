;;; Tests for 'selected-window-accent-mode'

(require 'selected-window-accent-mode)
(require 'ert) ;; Emacs Lisp's testing framework

;;; Testing helper functions

(ert-deftest test-selected-window-accent--color-name-to-hex ()
  "Test the conversion of color names to hex."
  (should (equal (selected-window-accent--color-name-to-hex "white") "#ffffff"))
  (should (equal (selected-window-accent--color-name-to-hex "black") "#000000"))
  (should (equal (selected-window-accent--color-name-to-hex "red") "#ff0000")))

(ert-deftest test-selected-window-accent--invert-color ()
  "Test inverting a color."
  (should (equal (selected-window-accent--invert-color "#ffffff") "#000000"))
  (should (equal (selected-window-accent--invert-color "#000000") "#ffffff"))
  (should (equal (selected-window-accent--invert-color "#123456") "#edcba9")))

(ert-deftest test-selected-window-accent--increment-color-brightness ()
  "Test incrementing the brightness of a color."
  (should (equal (selected-window-accent--increment-color-brightness "#000000" 0.1) "#191919"))
  (should (equal (selected-window-accent--increment-color-brightness "#123456" 0.1) "#263d60"))
  (should-error (selected-window-accent--increment-color-brightness "invalid-color" 0.1)))

(ert-deftest test-selected-window-accent--decrement-color-brightness ()
  "Test decrementing the brightness of a color."
  (should (equal (selected-window-accent--decrement-color-brightness "#ffffff" 0.1) "#e5e5e5"))
  (should (equal (selected-window-accent--decrement-color-brightness "#123456" 0.1) "#0e283f"))
  (should-error (selected-window-accent--decrement-color-brightness "invalid-color" 0.1)))

(ert-deftest test-selected-window-accent-blend-colors ()
  "Test blending two colors together."
  (should (equal (selected-window-accent-blend-colors "#000000" "#ffffff" 0.5) "#808080"))
  (should (equal (selected-window-accent-blend-colors "#ff0000" "#0000ff" 0.5) "#800080"))
  (should (equal (selected-window-accent-blend-colors "#123456" "#654321" 0.3) "#213a52")))

(ert-deftest test-selected-window-accent--more-than-one-window-p ()
  "Test whether more than one window is open in the current frame."
  ;; Mocking could be done here if needed for `window-list` behavior.
  (should-not (selected-window-accent--more-than-one-window-p))
  ;; Open a new window temporarily for testing
  (let ((initial-window (selected-window)))
    (split-window-right)
    (unwind-protect
        (should (selected-window-accent--more-than-one-window-p))
      (delete-other-windows))))

;;; Integration-level tests

(ert-deftest test-selected-window-accent ()
  "Test the main `selected-window-accent` function."
  (let ((selected-window-accent-mode-style 'tiling)
        (selected-window-accent-custom-color "#ff0000")
        (selected-window-accent-tab-accent t))
    ;; Mock environment validation
    (with-temp-buffer
      ;; Simulate environment changes
      (selected-window-accent)
      ;; Validate that the fringe face attributes were updated
      (should (equal (face-attribute 'fringe :background) "#ff0000"))
      ;; Validate tab accents
      (should (equal (face-attribute 'tab-bar-tab :background) "#ff0000")))))

(ert-deftest test-selected-window-accent-sync-tab-bar-to-theme ()
  "Test syncing tab-bar faces with the current theme."
  (let ((default-bg "#282c34")
        (default-fg "#ffffff")
        (inactive-fg "#d8dee9"))
    ;; Temporarily modify the default face attributes
    (set-face-attribute 'default nil :background default-bg :foreground default-fg)
    (set-face-attribute 'mode-line-inactive nil :foreground inactive-fg)
    ;; Call the sync function
    (selected-window-accent-sync-tab-bar-to-theme)
    ;; Check the updated attributes
    (should (equal (face-attribute 'tab-bar :background) default-bg))
    (should (equal (face-attribute 'tab-bar-tab :background) default-fg))
    (should (equal (face-attribute 'tab-bar-tab :foreground) default-bg))
    (should (equal (face-attribute 'tab-bar-tab-inactive :foreground) inactive-fg))))

(ert-deftest test-selected-window-accent-switch-selected-window-accent-style ()
  "Test switching the selected window accent style."
  (selected-window-accent-switch-selected-window-accent-style 'subtle)
  (should (eq selected-window-accent-mode-style 'subtle))
  ;; Ensure the style application
  (should-not (eq (face-attribute 'mode-line-active :height) 'unspecified)))

;;; Test toggles

(ert-deftest test-selected-window-accent-toggle-smart-borders ()
  "Test toggling the smart borders feature."
  (let ((selected-window-accent-smart-borders nil))
    (selected-window-accent-toggle-smart-borders)
    (should selected-window-accent-smart-borders)))

(ert-deftest test-selected-window-accent-toggle-tab-accent ()
  "Test toggling the tab accent feature."
  (let ((selected-window-accent-tab-accent nil))
    (selected-window-accent-toggle-tab-accent)
    (should selected-window-accent-tab-accent)))

(ert-deftest test-selected-window-accent-output-selected-window-accent-settings ()
  "Test exporting current settings into a buffer."
  (selected-window-accent-output-selected-window-accent-settings)
  (let ((output-buffer (get-buffer "*selected-window-accent-mode-settings*")))
    (should output-buffer)
    (with-current-buffer output-buffer
      (should (string-match "(use-package selected-window-accent-mode" (buffer-string))))))

;;; Run all tests

;; You can invoke this via `M-x ert` or programmatically
(when noninteractive
  (ert-run-tests-batch-and-exit))
```

### Key Notes:
1. **Helper Functions:**
   - Focus tests on small utility functions like `selected-window-accent--color-name-to-hex`, `selected-window-accent--invert-color`, etc., to ensure correctness independently of the larger GUI context.

2. **Mocking:**
   - Mocking functionality like `window-list` or face attributes might be optional during Emacs unit testing since the goal is to validate changes rather than simulate OS behavior.

3. **Buffer and Face Management:**
   - Temporary buffers and face modifications are used to ensure the test environment doesn't interfere with the user's session or other tests.

4. **Interactive Functions:**
   - For functions like `selected-window-accent-toggle-smart-borders`, toggling is tested by observing changes in related global variables' state.

5. **Test Execution:**
   - Use `M-x ert` for an interactive report or attach `(ert-run-tests-batch-and-exit)` for CI pipelines and batch modes.

With the above coverage, you address core functions, integration (effect application to the Emacs environment), and feature toggles.
