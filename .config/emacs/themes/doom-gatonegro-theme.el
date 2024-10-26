;;; doom-gatonegro-theme.el --- modified from doom-outrun-elecric.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-gatonegro-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-gatonegro-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-gatonegro-theme
  :type 'boolean)

(defcustom doom-gatonegro-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gatonegro-theme
  :type 'boolean)

(defcustom doom-gatonegro-comment-bg doom-gatonegro-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-gatonegro-theme
  :type 'boolean)

(defcustom doom-gatonegro-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gatonegro-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-gatonegro
  "A vibrant, neon colored theme."

  ;; name        default   256       16
  ((bg         '("#16161c" "#16161c" nil           ))
   (bg-alt     '("#1a1a26" "#1a1a26" nil           ))
   (base0      '("#1a1a30" "#1a1a30" "black"       ))
   (base1      '("#1a1a40" "#1a1a40" "brightblack" ))
   (base2      '("#2a2a50" "#2a2a50" "brightblack" ))
   (base3      '("#3a3a60" "#3a3a60" "brightblack" ))
   (base4      '("#2d2d50" "#2d2d50" "brightblack" ))
   (base5      '("#ff00aa" "#ff00aa" "brightblack" ))
   (base6      '("#5a5a90" "#5a5a90" "brightblack" ))
   (base7      '("#4a4a88" "#4a4a88" "brightblack" ))
   (base8      '("#8181bb" "#8181bb" "white"       ))
   (fg-alt     '("#7979bb" "#7979bb" "white"       ))
   (fg         '("#d1d1f1" "#d1d1f1" "brightwhite" ))

   (grey       '("#545a90" "#545a90" "gray"          ))
   (red        '("#ff00aa" "#ff00aa" "red"          ))
   (orange     '("#ff7700" "#ff7700" "brightred"    ))
   (green      '("#aaee00" "#aaee00" "green"        ))
   (teal       '("#0af5c2" "#0af5c2" "brightgreen"  ))
   (yellow     '("#e1ff00" "#e1ff00" "yellow"       ))
   (blue       '("#00c8ff" "#00c8ff" "brightblue"   ))
   (dark-blue  '("#0d8eff" "#0d8eff" "blue"         ))
   (magenta    '("#d724ff" "#d724ff" "magenta"      ))
   (violet     '("#ff00aa" "#ff00aa" "brightmagenta"))
   (cyan       '("#00ffe0" "#00ffe0" "brightcyan"   ))
   (dark-cyan  '("#00eaff" "#00eaff" "cyan"         ))
   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-gatonegro-brighter-comments blue grey))
   (doc-comments   teal)
   (constants      violet)
   (functions      cyan)
   (keywords       magenta)
   (methods        cyan)
   (operators      magenta)
   (type           yellow)
   (strings        fg-alt)
   (variables      violet)
   (numbers        yellow)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-gatonegro-brighter-modeline)
   (-modeline-pad
    (when doom-gatonegro-padded-modeline
      (if (integerp doom-gatonegro-padded-modeline) doom-gatonegro-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-gatonegro-comment-bg (doom-lighten bg 0.05) 'unspecified) :slant 'italic)
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-constant-face &override) :weight 'bold)
   ((font-lock-function-name-face &override) :foreground functions)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base6)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (mode-line-highlight :background magenta :foreground bg :weight 'bold)
   (vertical-border :foreground base5)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background magenta)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground magenta)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground magenta)
   ;;;; company
   (company-tooltip-selection :foreground green :background base2)
   (company-tooltip-common    :foreground red :distant-foreground base0 :weight 'bold)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background magenta)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :background base0)
   (org-hide              :foreground hidden)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-gatonegro-theme.el ends here
