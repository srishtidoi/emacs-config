;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "tecosaur"
      user-mail-address "tecosaur@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
(setq doom-font (font-spec :family "Fira Code" :size 18)
      doom-variable-pitch-font (font-spec :family "sans"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-vibrant)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(defun github-conversation-p (window-title)
(or (string-match-p "Pull Request" window-title)
    (string-match-p "Issue" window-title)
    ))
(defun ea-popup-handler (app-name window-title x y w h)
(set-frame-size (selected-frame) 80 12)
; font
(interactive)
(setq buffer-face-mode-face '(:family "P22 Underground Book" :height 160))
(buffer-face-mode)
; position
(let* ((mousepos (split-string (shell-command-to-string "xdotool getmouselocation | sed -E \"s/ screen:0 window:[^ ]*|x:|y://g\"")))
        (mouse-x (- (string-to-number (nth 0 mousepos)) 100))
        (mouse-y (- (string-to-number (nth 1 mousepos)) 50)))
    (set-frame-position (selected-frame) mouse-x mouse-y))
; set major mode
(cond
    ((github-conversation-p window-title) (gfm-mode))
    (t (markdown-mode)) ; default major mode
    )
; start in insert
(evil-insert-state)
)
(add-hook 'ea-popup-hook 'ea-popup-handler)

(after! centaur-tabs
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-set-bar 'under)
        centaur-tabs-gray-out-icons 'buffer
  ;;       centaur-tabs-style "bar"
  ;;       centaur-tabs-set-modified-marker t
  ;; ;; (centaur-tabs-headline-match)
  ;; (centaur-tabs-mode t)
  (centaur-tabs-change-fonts "P22 Underground Book" 160))
(setq x-underline-at-descent-line t)

;; Autocompletion is nice

(after! company
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1)
  (setq company-show-numbers t))

;; (add-to-list 'company-backends #'company-tabnine)

(set-company-backend! '(text-mode
                        markdown-mode
                        gfm-mode)
  '(:seperate company-ispell
              company-files
              company-yasnippet))

;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
;; but I prefer hard code the dictionary path. That's more portable.
(setq company-ispell-dictionary (file-truename "~/.config/Code/User/Custom cSpell Dictionaries/SCOWL-workdlist-au-uk-60.txt"))

;; red is for bad things, unsaved changes isn't inherently bad!
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(prettify-utils-add-hook LaTeX-mode
                         ("\\part" "§")
                         ("\\section" "§§")
                         ("\\subsection" "§§§")
                         ("\\subsubsection" "§§§§")
                         ("\\paragraph" "¶")
                         ("\\subparagraph" "¶¶"))
