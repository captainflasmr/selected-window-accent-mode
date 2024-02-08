This document contains the release notes for each tagged commit on the project&rsquo;s main git repository: <https://github.com/captainflasmr/selected-window-accent-mode>.

With a roadmap and issues also supplied


# ISSUES


## TODO Do not apply highlighting when frame only contains 1 window


# ROADMAP


## TODO add darken desaturated and tab highlight examples to README


## TODO define accent color hue adjustment


## TODO define compensating margin


## TODO Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the &rsquo;default mode.


## TODO header-line not shown on window split - I have a funny feeling this could be very difficult, if not impossible!


## TODO restore modeline height when switching between modes


## TODO adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.


## TODO excess selected-window disruption in header-line. (not sure I can do much about this)


## TODO define which theme face attribute to use as the main accent color

Currently the default is to use the `highlight` face


## WATCH possible overheads of updating visual elements for each window?


## WATCH careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.


# Latest TBD


## DONE define accent color saturation adjustment


## DONE define accent color darken adjustment


## DONE highlight selected tab with same accent color


## DONE add to MELPA


# Version 0.5.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>


## DONE minor change to properly format color-theme-buffer-local


# Version 0.4.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>


## DONE pacified package-lint with visual-fill-column 0.0


## DONE Added similar package comparisons as suggested


# Version 0.3.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>


## DONE rename color-name-to-hex to selected-window-accent&#x2013;color-name-to-hex


# Version 0.2.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>


## DONE Fixing issues to be able to submit to MELPA

-   byte-compile / flycheck
-   checkdoc
-   package-lint
-   other


## DONE images to img directory and referenced from README


# Version 0.1.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-12 Fri&gt;</span></span>

This is considered beta.


## DONE expand emacs help / documentation


## DONE add GNU header


## DONE cope better with 0 thickness


## DONE Add ChangeLog.


## DONE visual-fill-column-mode not working again!


## DONE improve modeline contrast between fg and bg


# Testing

developing locally using:

```elisp
(use-package selected-window-accent-mode
   :load-path "~/repos/selected-window-accent-mode"
```

commiting to github then removing ~/.config/emacs/elpa/selected-window-accent-mode, changing emacs init to:

```elisp
(use-package selected-window-accent-mode
  :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode")
```

restart emacs and test
