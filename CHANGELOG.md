- [ISSUES](#orgda92a92)
- [ROADMAP](#orgc420dc3)
  - [add darken desaturated and tab highlight examples to README](#org08f79ad)
  - [define accent color hue adjustment](#org2e563cb)
  - [define compensating margin](#org892bd95)
  - [Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the &rsquo;default mode.](#org23e0182)
  - [header-line not shown on window split - I have a funny feeling this could be very difficult, if not impossible!](#org8b3a220)
  - [restore modeline height when switching between modes](#org89e7f31)
  - [adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.](#org811f70e)
  - [excess selected-window disruption in header-line. (not sure I can do much about this)](#orga8fda15)
  - [define which theme face attribute to use as the main accent color](#org7e31d0a)
  - [possible overheads of updating visual elements for each window?](#org590d6a1)
  - [careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.](#org1f00a48)
- [Latest TBD](#org52fa783)
  - [ISSUE #1 Do not apply highlighting when frame only contains 1 window](#org5864ba9)
  - [define accent color saturation adjustment](#org4182d95)
  - [define accent color darken adjustment](#org8381e67)
  - [highlight selected tab with same accent color](#orgf7c8913)
  - [add to MELPA](#orge6f5650)
- [Version 0.5.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>](#org0bfae3e)
  - [minor change to properly format color-theme-buffer-local](#org97c6ead)
- [Version 0.4.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>](#org19cc62f)
  - [pacified package-lint with visual-fill-column 0.0](#org080d569)
  - [Added similar package comparisons as suggested](#org30d7973)
- [Version 0.3.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>](#org04af64d)
  - [rename color-name-to-hex to selected-window-accent&#x2013;color-name-to-hex](#orgfff9f23)
- [Version 0.2.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>](#orgd255d8b)
  - [Fixing issues to be able to submit to MELPA](#org8ad9271)
  - [images to img directory and referenced from README](#orgb21600f)
- [Version 0.1.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-12 Fri&gt;</span></span>](#org1ac406c)
  - [expand emacs help / documentation](#org8becb8a)
  - [add GNU header](#org98073e8)
  - [cope better with 0 thickness](#org2cc1a11)
  - [Add ChangeLog.](#org9830ee3)
  - [visual-fill-column-mode not working again!](#orge85e217)
  - [improve modeline contrast between fg and bg](#org23ff822)
- [Testing](#orgc3ead6b)

This document contains the release notes for each tagged commit on the project&rsquo;s main git repository: <https://github.com/captainflasmr/selected-window-accent-mode>.

With a roadmap and issues also supplied


<a id="orgda92a92"></a>

# ISSUES


<a id="orgc420dc3"></a>

# ROADMAP


<a id="org08f79ad"></a>

## TODO add darken desaturated and tab highlight examples to README


<a id="org2e563cb"></a>

## TODO define accent color hue adjustment


<a id="org892bd95"></a>

## TODO define compensating margin


<a id="org23e0182"></a>

## TODO Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the &rsquo;default mode.


<a id="org8b3a220"></a>

## TODO header-line not shown on window split - I have a funny feeling this could be very difficult, if not impossible!


<a id="org89e7f31"></a>

## TODO restore modeline height when switching between modes


<a id="org811f70e"></a>

## TODO adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.


<a id="orga8fda15"></a>

## TODO excess selected-window disruption in header-line. (not sure I can do much about this)


<a id="org7e31d0a"></a>

## TODO define which theme face attribute to use as the main accent color

Currently the default is to use the `highlight` face


<a id="org590d6a1"></a>

## WATCH possible overheads of updating visual elements for each window?


<a id="org1f00a48"></a>

## WATCH careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.


<a id="org52fa783"></a>

# Latest TBD


<a id="org5864ba9"></a>

## DONE ISSUE #1 Do not apply highlighting when frame only contains 1 window


<a id="org4182d95"></a>

## DONE define accent color saturation adjustment


<a id="org8381e67"></a>

## DONE define accent color darken adjustment


<a id="orgf7c8913"></a>

## DONE highlight selected tab with same accent color


<a id="orge6f5650"></a>

## DONE add to MELPA


<a id="org0bfae3e"></a>

# Version 0.5.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>


<a id="org97c6ead"></a>

## DONE minor change to properly format color-theme-buffer-local


<a id="org19cc62f"></a>

# Version 0.4.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>


<a id="org080d569"></a>

## DONE pacified package-lint with visual-fill-column 0.0


<a id="org30d7973"></a>

## DONE Added similar package comparisons as suggested


<a id="org04af64d"></a>

# Version 0.3.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>


<a id="orgfff9f23"></a>

## DONE rename color-name-to-hex to selected-window-accent&#x2013;color-name-to-hex


<a id="orgd255d8b"></a>

# Version 0.2.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>


<a id="org8ad9271"></a>

## DONE Fixing issues to be able to submit to MELPA

-   byte-compile / flycheck
-   checkdoc
-   package-lint
-   other


<a id="orgb21600f"></a>

## DONE images to img directory and referenced from README


<a id="org1ac406c"></a>

# Version 0.1.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-12 Fri&gt;</span></span>

This is considered beta.


<a id="org8becb8a"></a>

## DONE expand emacs help / documentation


<a id="org98073e8"></a>

## DONE add GNU header


<a id="org2cc1a11"></a>

## DONE cope better with 0 thickness


<a id="org9830ee3"></a>

## DONE Add ChangeLog.


<a id="orge85e217"></a>

## DONE visual-fill-column-mode not working again!


<a id="org23ff822"></a>

## DONE improve modeline contrast between fg and bg


<a id="orgc3ead6b"></a>

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
