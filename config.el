;;; config.el -*- lexical-binding: t; -*-
(setq user-full-name "tecosaur"
      user-mail-address "tecosaur@gmail.com")
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq doom-font (font-spec :family "Fira Code" :size 18)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 20))
(setq doom-theme 'doom-vibrant)
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
(setq display-line-numbers-type 'relative)
(defun add-mouse-back-button-to-Info-mode ()
  "Add some browser styled nav keys for `Info-mode'."
  (local-set-key (kbd "<mouse-8>") 'Info-history-back)
  (local-set-key (kbd "<mouse-9>") 'Info-history-forward))

(add-hook 'Info-mode-hook 'add-mouse-back-button-to-Info-mode)
(after! centaur-tabs
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-set-bar 'above)
        centaur-tabs-gray-out-icons 'buffer
  (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (setq company-show-numbers t))
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-company-backend! '(text-mode
                        markdown-mode
                        gfm-mode)
  '(:seperate company-ispell
              company-files
              company-yasnippet))
(setq company-ispell-dictionary (file-truename "~/.config/Code/User/Custom cSpell Dictionaries/SCOWL-workdlist-au-uk-60.txt"))
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
(setq org-directory "~/Desktop/TEC/Other/org")
(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))
(add-hook 'org-mode-hook '+org-pretty-mode)
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
   background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "
<style type=\"text/css\">
   :root {
      --theme-bg: %s;
      --theme-bg-alt: %s;
      --theme-base0: %s;
      --theme-base1: %s;
      --theme-base2: %s;
      --theme-base3: %s;
      --theme-base4: %s;
      --theme-base5: %s;
      --theme-base6: %s;
      --theme-base7: %s;
      --theme-base8: %s;
      --theme-fg: %s;
      --theme-fg-alt: %s;
      --theme-grey: %s;
      --theme-red: %s;
      --theme-orange: %s;
      --theme-green: %s;
      --theme-teal: %s;
      --theme-yellow: %s;
      --theme-blue: %s;
      --theme-dark-blue: %s;
      --theme-magenta: %s;
      --theme-violet: %s;
      --theme-cyan: %s;
      --theme-dark-cyan: %s;
   }
</style>"
       (doom-color 'bg)
       (doom-color 'bg-alt)
       (doom-color 'base0)
       (doom-color 'base1)
       (doom-color 'base2)
       (doom-color 'base3)
       (doom-color 'base4)
       (doom-color 'base5)
       (doom-color 'base6)
       (doom-color 'base7)
       (doom-color 'base8)
       (doom-color 'fg)
       (doom-color 'fg-alt)
       (doom-color 'grey)
       (doom-color 'red)
       (doom-color 'orange)
       (doom-color 'green)
       (doom-color 'teal)
       (doom-color 'yellow)
       (doom-color 'blue)
       (doom-color 'dark-blue)
       (doom-color 'magenta)
       (doom-color 'violet)
       (doom-color 'cyan)
       (doom-color 'dark-cyan))
        "
<link rel='stylesheet' type='text/css' href'https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css'/>
<link rel='stylesheet' type='text/css' href='https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css'/>

<script src='https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js'></script>
<script src='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js'></script>
<script type='text/javascript' src='https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js'></script>
<script type='text/javascript' src='https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js'></script>

<style>
   pre.src {
     background-color: var(--theme-bg);
     color: var(--theme-fg);
     scrollbar-color:#bbb6#9992;
     scrollbar-width: thin;
     margin: 0;
     border: none;
   }
   div.org-src-container {
     border-radius: 12px;
     overflow: hidden;
     margin-bottom: 24px;
     margin-top: 1px;
     border: 1px solid#e1e4e5;
   }
   pre.src::before {
     background-color:#6666;
     top: 8px;
     border: none;
     border-radius: 5px;
     line-height: 1;
     border: 2px solid var(--theme-bg);
     opacity: 1;
     transition: opacity 200ms;
   }
   pre.src:active::before { opacity: 0; }

   code {
     border-radius: 5px;
     background:#e8e8e8;
     font-size: 80%;
   }

   kbd {
     display: inline-block;
     padding: 3px 5px;
     font: 80% SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;
     line-height: normal;
     line-height: 10px;
     color:#444d56;
     vertical-align: middle;
     background-color:#fafbfc;
     border: 1px solid#d1d5da;
     border-radius: 3px;
     box-shadow: inset 0 -1px 0#d1d5da;
   }

   table {
     max-width: 100%;
     overflow-x: auto;
     display: block;
     border-top: none;
   }

   \#table-of-contents { overflow-y: auto; }
   blockquote p { margin: 8px 0px 16px 0px; }
   \#postamble .date { color: var(--theme-green); }

   ::-webkit-scrollbar { width: 10px; height: 8px; }
   ::-webkit-scrollbar-track { background:#9992; }
   ::-webkit-scrollbar-thumb { background:#ccc; border-radius: 10px; }
   ::-webkit-scrollbar-thumb:hover { background:#888; }
</style>
"
        ))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)
(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<code>%s</code>")
        (italic . "<i>%s</i>")
        (strike-through . "<del>%s</del>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<kbd>%s</kbd>")))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("fancy-article"
               "\\documentclass{scrartcl}\n\\usepackage[default,sfdefault]{lato}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "fancy-article")

   (add-to-list 'org-latex-packages-alist '("" "minted"))
   (setq org-latex-listings 'minted)
   (setq org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}, colorlinks=true}\n\\urlstyle{same}\n")
   (setq org-latex-pdf-process
         '("latexmk -shell-escape -interaction=nonstopmode -f -pdf -output-directory=%o %f")))
(eval-after-load "org"
  '(require 'ox-gfm nil t))
