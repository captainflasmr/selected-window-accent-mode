- [ISSUES](#org756beac)
  - [Do not apply highlighting when frame only contains 1 window](#org6a40377)
- [ROADMAP](#org4a086f7)
  - [add darken desaturated and tab highlight examples to README](#orgf483518)
  - [define accent color hue adjustment](#orgd22c9cf)
  - [define compensating margin](#orgfda283d)
  - [Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the &rsquo;default mode.](#org21d3773)
  - [header-line not shown on window split - I have a funny feeling this could be very difficult, if not impossible!](#org7975b4f)
  - [restore modeline height when switching between modes](#org354da72)
  - [adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.](#org4ef7ed0)
  - [excess selected-window disruption in header-line. (not sure I can do much about this)](#org6e050ab)
  - [define which theme face attribute to use as the main accent color](#org1b6b9f1)
  - [possible overheads of updating visual elements for each window?](#org97e676c)
  - [careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.](#org068c404)
- [Latest TBD](#orga246ae9)
  - [define accent color saturation adjustment](#orgb3bc513)
  - [define accent color darken adjustment](#org30122fd)
  - [highlight selected tab with same accent color](#org523959b)
  - [add to MELPA](#org1918a65)
- [Version 0.5.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>](#org0c3ec6e)
  - [minor change to properly format color-theme-buffer-local](#orgb26bb5b)
- [Version 0.4.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>](#org4a3b2bc)
  - [pacified package-lint with visual-fill-column 0.0](#org56a8fce)
  - [Added similar package comparisons as suggested](#org2aaf18c)
- [Version 0.3.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>](#org8ba6d8a)
  - [rename color-name-to-hex to selected-window-accent&#x2013;color-name-to-hex](#org5946ee9)
- [Version 0.2.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>](#orgcb48101)
  - [Fixing issues to be able to submit to MELPA](#org3115c7c)
  - [images to img directory and referenced from README](#orgade500b)
- [Version 0.1.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-12 Fri&gt;</span></span>](#orgf1a113f)
  - [expand emacs help / documentation](#org539f2ca)
  - [add GNU header](#org15a02a2)
  - [cope better with 0 thickness](#org8650a3c)
  - [Add ChangeLog.](#orgc1497a8)
  - [visual-fill-column-mode not working again!](#org0234c48)
  - [improve modeline contrast between fg and bg](#org6665584)
- [Testing](#org88f5f0d)

This document contains the release notes for each tagged commit on the project&rsquo;s main git repository: <https://github.com/captainflasmr/selected-window-accent-mode>.

With a roadmap and issues also supplied


<a id="org756beac"></a>

# ISSUES


<a id="org6a40377"></a>

## TODO Do not apply highlighting when frame only contains 1 window


<a id="org4a086f7"></a>

# ROADMAP


<a id="orgf483518"></a>

## TODO add darken desaturated and tab highlight examples to README


<a id="orgd22c9cf"></a>

## TODO define accent color hue adjustment


<a id="orgfda283d"></a>

## TODO define compensating margin


<a id="org21d3773"></a>

## TODO Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the &rsquo;default mode.


<a id="org7975b4f"></a>

## TODO header-line not shown on window split - I have a funny feeling this could be very difficult, if not impossible!


<a id="org354da72"></a>

## TODO restore modeline height when switching between modes


<a id="org4ef7ed0"></a>

## TODO adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.


<a id="org6e050ab"></a>

## TODO excess selected-window disruption in header-line. (not sure I can do much about this)


<a id="org1b6b9f1"></a>

## TODO define which theme face attribute to use as the main accent color

Currently the default is to use the `highlight` face


<a id="org97e676c"></a>

## WATCH possible overheads of updating visual elements for each window?


<a id="org068c404"></a>

## WATCH careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.


<a id="orga246ae9"></a>

# Latest TBD


<a id="orgb3bc513"></a>

## DONE define accent color saturation adjustment


<a id="org30122fd"></a>

## DONE define accent color darken adjustment


<a id="org523959b"></a>

## DONE highlight selected tab with same accent color


<a id="org1918a65"></a>

## DONE add to MELPA


<a id="org0c3ec6e"></a>

# Version 0.5.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>


<a id="orgb26bb5b"></a>

## DONE minor change to properly format color-theme-buffer-local


<a id="org4a3b2bc"></a>

# Version 0.4.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-29 Mon&gt;</span></span>


<a id="org56a8fce"></a>

## DONE pacified package-lint with visual-fill-column 0.0


<a id="org2aaf18c"></a>

## DONE Added similar package comparisons as suggested


<a id="org8ba6d8a"></a>

# Version 0.3.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>


<a id="org5946ee9"></a>

## DONE rename color-name-to-hex to selected-window-accent&#x2013;color-name-to-hex


<a id="orgcb48101"></a>

# Version 0.2.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-25 Thu&gt;</span></span>


<a id="org3115c7c"></a>

## DONE Fixing issues to be able to submit to MELPA

-   byte-compile / flycheck
-   checkdoc
-   package-lint
-   other


<a id="orgade500b"></a>

## DONE images to img directory and referenced from README


<a id="orgf1a113f"></a>

# Version 0.1.0 on <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-12 Fri&gt;</span></span>

This is considered beta.


<a id="org539f2ca"></a>

## DONE expand emacs help / documentation


<a id="org15a02a2"></a>

## DONE add GNU header


<a id="org8650a3c"></a>

## DONE cope better with 0 thickness


<a id="orgc1497a8"></a>

## DONE Add ChangeLog.


<a id="org0234c48"></a>

## DONE visual-fill-column-mode not working again!


<a id="org6665584"></a>

## DONE improve modeline contrast between fg and bg


<a id="org88f5f0d"></a>

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
