;;; config.el -*- lexical-binding: t; -*-
(setq user-full-name "tecosaur"
      user-mail-address "tecosaur@gmail.com")
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t)                        ; Nobody likes to loose work, I certainly don't

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words
(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
;; (setq-default major-mode 'org-mode)
(setq doom-font (font-spec :family "Fira Code" :size 22)
      doom-big-font (font-spec :family "Fira Code" :size 36)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 24))
(setq doom-theme 'doom-vibrant)
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
(setq display-line-numbers-type 'relative)
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
(custom-set-faces! '(doom-modeline-evil-insert-state :weight bold :foreground "#339CDB"))
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
  (setq save-abbrevs 'silently))
(after! centaur-tabs
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above)
        centaur-tabs-gray-out-icons 'buffer
  (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-company-backend! '(text-mode
                        markdown-mode
                        gfm-mode)
  '(:seperate company-ispell
              company-files
              company-yasnippet))
(setq company-ispell-dictionary (file-truename "~/.config/Code/User/Custom cSpell Dictionaries/SCOWL-workdlist-au-uk-60.txt"))
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))
(defun markdown-window-p (window-title)
  "Judges from WINDOW-TITLE whether the current window likes markdown"
  (or (string-match-p "Pull Request" window-title)
      (string-match-p "Issue" window-title)
      (string-match-p "Discord" window-title)))
(defun ea-popup-handler (app-name window-title x y w h)
  (set-frame-size (selected-frame) 80 12)
  (interactive)
  ; position
  (let* ((mousepos (split-string (shell-command-to-string "xdotool getmouselocation | sed -E \"s/ screen:0 window:[^ ]*|x:|y://g\"")))
         (mouse-x (- (string-to-number (nth 0 mousepos)) 100))
         (mouse-y (- (string-to-number (nth 1 mousepos)) 50)))
    (set-frame-position (selected-frame) mouse-x mouse-y))

  (set-frame-name (concat "Quick Edit ∷ " ea-app-name " — "
                          (truncate-string-to-width (string-trim  (string-trim-right window-title (format "-[A-Za-z0-9 ]*%s" ea-app-name)) "[\s-]+" "[\s-]+") 45 nil nil "…")))

  ; set major mode
  (cond
   ((markdown-window-p window-title) (gfm-mode))
   (t (org-mode)) ; default major mode
   )

  (when (gui-get-selection 'PRIMARY) (insert (gui-get-selection 'PRIMARY)))

  (evil-insert-state) ; start in insert
  (when (centaur-tabs-mode) (centaur-tabs-local-mode t)) ; disable tabs

  ; I'm used to C-c C-c for 'completed the thing' so add that
  (local-set-key (kbd "C-c C-c") (lambda () (interactive) (local-unset-key (kbd "C-c C-c")) (delete-frame))))

(add-hook 'ea-popup-hook 'ea-popup-handler)
(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1))
(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; defult + 
(setq treemacs-ignore-suffixes '(;; AucTeC
                                   ".auctex-auto"
                                   "_region_.log"
                                   "_region_.tex"
                                   ;; LaTeX
                                   ".aux"
                                   ".ptc"
                                   ".fdb_latexmk"
                                   ".fls"
                                   ".synctex.gz"
                                   ".toc"
                                   ;; LaTeX - glossary
                                   ".glg"
                                   ".glo"
                                   ".gls"
                                   ".glsdefs"
                                   ".ist"
                                   ".acn"
                                   ".acr"
                                   ".alg"
                                   ;; LaTeX - pgfplots
                                   ".mw"
                                   ;; LaTeX - pdfx
                                   "pdfa.xmpi"
                                   ))
(setq treemacs-ignore-prefixes '(;; LaTeX
                                 "_minted-"))
(after! treemacs
  (defun treemacs-ignore-filter (file _)
    (let ((ignore-file nil))
      (dolist (suffix treemacs-ignore-suffixes ignore-file)
        (setq ignore-file (or ignore-file (s-suffix? suffix file))))
      (dolist (prefix treemacs-ignore-prefixes ignore-file)
        (setq ignore-file (or ignore-file (s-prefix? prefix file))))
      ))
  (push #'treemacs-ignore-filter treemacs-ignored-file-predicates))
(setq calc-angle-mode 'rad)
(electric-pair-mode t)
(setq ispell-dictionary "en_GBs_au_SCOWL_80_0_k_hr")
(setq spray-wpm 500
      spray-height 700)
(add-hook 'doom-load-theme-hook 'theme-magic-from-emacs)
(setq wttrin-default-cities '(""))
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(setq org-directory "~/.org"                      ; let's put files here
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convininet
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-catch-invisible-edits 'smart)           ; try not to accidently do weird stuff in invisible regions
(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
  (:prefix "b"
    :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
(def-package! org-ref
   :after org
   :config
    (setq org-ref-completion-library 'org-ref-ivy-cite))
(after! org (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)
(setq global-org-pretty-table-mode t)
(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.2)
  '(outline-2 :weight bold :height 1.12)
  '(outline-3 :weight bold :height 1.1)
  '(outline-4 :weight semi-bold :height 1.08)
  '(outline-5 :weight semi-bold :height 1.05)
  '(outline-6 :weight semi-bold :height 1.02)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))
(setq org-ellipsis " ▾ "
      org-bullets-bullet-list '("◉" "○" "✸" "✿" "✤")
      ;; org-bullets-bullet-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
      )
(after! org
  (appendq! +pretty-code-symbols
            '(:checkbox    "☐"
              :pending     "◼"
              :checkedbox  "☑"
              :results     "🠶"
              :property    "☸"
              :properties  "⚙"
              :end         "∎"
              :options     "⌥"
              :title       "𝙏"
              :author      "𝘼"
              :date        "𝘿"
              :begin_quote "❮"
              :end_quote   "❯"
              :em_dash     "—"))
  (set-pretty-symbols! 'org-mode
    :merge t
    :checkbox    "[ ]"
    :pending     "[-]"
    :checkedbox  "[X]"
    :results     "#+RESULTS:"
    :property    "#+PROPERTY:"
    :property    ":PROPERTIES:"
    :end         ":END:"
    :options     "#+OPTIONS:"
    :title       "#+TITLE:"
    :author      "#+AUTHOR:"
    :date        "#+DATE:"
    :begin_quote "#+BEGIN_QUOTE"
    :end_quote   "#+END_QUOTE"
    :em_dash     "---")
)
(plist-put +pretty-code-symbols :name "⁍") ; or › could be good?
(add-hook 'org-mode-hook 'org-fragtog-mode)
(setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}

\\usepackage[T1]{fontenc}
\\usepackage{mathtools}
\\usepackage{textcomp,amssymb}
\\usepackage[makeroom]{cancel}

\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
% my custom stuff
\\usepackage{arev}
\\usepackage{arevmath}")
(after! org
;; make background of fragments transparent
;; (let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
;;   (plist-put dvipng--plist :use-xcolor t)
;;   (plist-put dvipng--plist :image-converter '("dvipng -D %D -bg 'transparent' -T tight -o %O %f")))
  (add-hook! 'doom-load-theme-hook
    (defun +org-refresh-latex-background ()
      (plist-put! org-format-latex-options
                  :background
                  (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                      'default)
                                  :background nil t))))
)
(after! org
  (add-to-list 'org-latex-regexps '("\\ce" "^\\\\ce{\\(?:[^\000{}]\\|{[^\000}]+?}\\)}" 0 nil)))
(after! org
  (defun scimax-org-latex-fragment-justify (justification)
    "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
    (interactive
     (list (intern-soft
            (completing-read "Justification (left): " '(left center right)
                             nil t nil nil 'left))))
    (let* ((ov (ov-at))
           (beg (ov-beg ov))
           (end (ov-end ov))
           (shift (- beg (line-beginning-position)))
           (img (overlay-get ov 'display))
           (img (and (and img (consp img) (eq (car img) 'image)
                          (image-type-available-p (plist-get (cdr img) :type)))
                     img))
           space-left offset)
      (when (and img
                 ;; This means the equation is at the start of the line
                 (= beg (line-beginning-position))
                 (or
                  (string= "" (s-trim (buffer-substring end (line-end-position))))
                  (eq 'latex-environment (car (org-element-context)))))
        (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
              offset (floor (cond
                             ((eq justification 'center)
                              (- (/ space-left 2) shift))
                             ((eq justification 'right)
                              (- space-left shift))
                             (t
                              0))))
        (when (>= offset 0)
          (overlay-put ov 'before-string (make-string offset ?\ ))))))

  (defun scimax-org-latex-fragment-justify-advice (beg end image imagetype)
    "After advice function to justify fragments."
    (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))


  (defun scimax-toggle-latex-fragment-justification ()
    "Toggle if LaTeX fragment justification options can be used."
    (interactive)
    (if (not (get 'scimax-org-latex-fragment-justify-advice 'enabled))
        (progn
          (advice-add 'org--format-latex-make-overlay :after 'scimax-org-latex-fragment-justify-advice)
          (put 'scimax-org-latex-fragment-justify-advice 'enabled t)
          (message "Latex fragment justification enabled"))
      (advice-remove 'org--format-latex-make-overlay 'scimax-org-latex-fragment-justify-advice)
      (put 'scimax-org-latex-fragment-justify-advice 'enabled nil)
      (message "Latex fragment justification disabled"))))
;; Numbered equations all have (1) as the number for fragments with vanilla
;; org-mode. This code injects the correct numbers into the previews so they
;; look good.
(after! org
  (defun scimax-org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
          (counter -1)
          (numberp))
      (setq results (loop for (begin .  env) in
                          (org-element-map (org-element-parse-buffer) 'latex-environment
                            (lambda (env)
                              (cons
                               (org-element-property :begin env)
                               (org-element-property :value env))))
                          collect
                          (cond
                           ((and (string-match "\\\\begin{equation}" env)
                                 (not (string-match "\\\\tag{" env)))
                            (incf counter)
                            (cons begin counter))
                           ((string-match "\\\\begin{align}" env)
                            (prog2
                                (incf counter)
                                (cons begin counter)
                              (with-temp-buffer
                                (insert env)
                                (goto-char (point-min))
                                ;; \\ is used for a new line. Each one leads to a number
                                (incf counter (count-matches "\\\\$"))
                                ;; unless there are nonumbers.
                                (goto-char (point-min))
                                (decf counter (count-matches "\\nonumber")))))
                           (t
                            (cons begin nil)))))

      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))

    (apply orig-func args))


  (defun scimax-toggle-latex-equation-numbering ()
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (not (get 'scimax-org-renumber-environment 'enabled))
        (progn
          (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
          (put 'scimax-org-renumber-environment 'enabled t)
          (message "Latex numbering enabled"))
      (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
      (put 'scimax-org-renumber-environment 'enabled nil)
      (message "Latex numbering disabled.")))

  (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
  (put 'scimax-org-renumber-environment 'enabled t))
(after! org (setq org-export-headline-levels 5)) ; I like nesting
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
<link rel='stylesheet' type='text/css' href='https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css'/>
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
     opacity: 0;
     transition: opacity 200ms;
   }
   pre.src:hover::before { opacity: 1; }
   pre.src:active::before { opacity: 0; }

   pre.example {
     border-radius: 12px;
     background: var(--theme-bg-alt);
     color: var(--theme-fg);
   }

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

   a {
       text-decoration: none;
       background-image: linear-gradient(#d8dce9, #d8dce9);
       background-position: 0% 100%;
       background-repeat: no-repeat;
       background-size: 0% 2px;
       transition: background-size .3s;
   }
   \#table-of-contents a {
       background-image: none;
   }
   a:hover, a:focus {
       background-size: 100% 2px;
   }
   a[href^='#'] { font-variant-numeric: oldstyle-nums; }
   a[href^='#']:visited { color:#3091d1; }

   li .checkbox {
       display: inline-block;
       width: 0.9em;
       height: 0.9em;
       border-radius: 3px;
       margin: 3px;
       top: 4px;
       position: relative;
   }
   li.on > .checkbox { background: var(--theme-green); box-shadow: 0 0 2px var(--theme-green); }
   li.trans > .checkbox { background: var(--theme-orange); box-shadow: 0 0 2px var(--theme-orange); }
   li.off > .checkbox { background: var(--theme-red); box-shadow: 0 0 2px var(--theme-red); }
   li.on > .checkbox::after {
     content: '';
     height: 0.45em;
     width: 0.225em;
     -webkit-transform-origin: left top;
     transform-origin: left top;
     transform: scaleX(-1) rotate(135deg);
     border-right: 2.8px solid#fff;
     border-top: 2.8px solid#fff;
     opacity: 0.9;
     left: 0.10em;
     top: 0.45em;
     position: absolute;
   }
   li.trans > .checkbox::after {
       content: '';
       font-weight: bold;
       font-size: 1.6em;
       position: absolute;
       top: 0.23em;
       left: 0.09em;
       width: 0.35em;
       height: 0.12em;
       background:#fff;
       opacity: 0.9;
       border-radius: 0.1em;
   }
   li.off > .checkbox::after {
    content: '✖';
    color:#fff;
    opacity: 0.9;
    position: relative;
    top: -0.40rem;
    left: 0.17em;
    font-size: 0.75em;
  }

   span.timestamp {
       color: #003280;
       background: #647CFF44;
       border-radius: 3px;
       line-height: 1.25;
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
(after! org
(appendq! org-html-checkbox-types '((html-span .
	  ((on . "<span class='checkbox'></span>")
	  (off . "<span class='checkbox'></span>")
	  (trans . "<span class='checkbox'></span>")))))
(setq org-html-checkbox-type 'html-span))
;; (setq-default org-html-with-latex `dvisvgm)
;; TODO make this /only/ apply to text (i.e. not URL)
(after! org
  (defun tec/org-export-latex-filter-acronym (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (let ((case-fold-search nil))
        (replace-regexp-in-string
         ";?\\b[A-Z][A-Z]+s?"
         (lambda (all-caps-str)
           ; only \acr if str doesn't start with ";"
           (if (equal (aref all-caps-str 0) 59) (substring all-caps-str 1)
             (if (equal (aref all-caps-str (- (length all-caps-str) 1)) ?s)
                 (concat "\\textls*[70]{\\textsc{" (s-downcase (substring all-caps-str 0 -1)) "}\\protect\\scalebox{.91}[.84]{s}}")
               (concat "\\textls*[70]{\\textsc{" (s-downcase all-caps-str) "}}"))))
         text t t))))

  (add-to-list 'org-export-filter-plain-text-functions
               'tec/org-export-latex-filter-acronym)
  (add-to-list 'org-export-filter-headline-functions
               'tec/org-export-latex-filter-acronym))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("fancy-article"
               "\\documentclass{scrartcl}\n\\usepackage[T1]{fontenc}\\usepackage[osf,largesc,helvratio=0.9]{newpxtext}\\usepackage[scale=0.92]{sourcecodepro}\n\\usepackage[varbb]{newpxmath}\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("blank"
               "[NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("bmc-article"
               "\\documentclass[article,code,maths]{bmc}
               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("bmc"
               "\\documentclass[code,maths]{bmc}
               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "bmc-article")

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("linenos" "")
          ("breakanywhere" "true")
          ("breakautoindent" "true")
          ("breaklines" "true")
          ("autogobble" "true")
          ("obeytabs" "true")
          ("python3" "true")
          ("breakbefore" "\\\\\\.+")
          ("breakafter" "\\,")
          ("style" "autumn")
          ("breaksymbol" "\\tiny\\ensuremath{\\hookrightarrow}")
          ("breakanywheresymbolpre" "\\,\\footnotesize\\ensuremath{{}_{\\rfloor}}")
          ("breakbeforesymbolpre" "\\,\\footnotesize\\ensuremath{{}_{\\rfloor}}")
          ("breakaftersymbolpre" "\\,\\footnotesize\\ensuremath{{}_{\\rfloor}}")))

  (setq org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true,\nlinkcolor=}\n\\urlstyle{same}\n")
  (setq org-latex-pdf-process
        '("latexmk -shell-escape -interaction=nonstopmode -f -pdf -output-directory=%o %f")))
(setq org-beamer-theme "[progressbar=foot]metropolis")

(setq org-beamer-frame-level 2)
(eval-after-load "org"
  '(require 'ox-gfm nil t))
(setq org-babel-python-command "python3")
(defun tec-org-python ()
  (if (eq major-mode 'python-mode)
   (progn (anaconda-mode t)
          (company-mode t)))
  )
(add-hook 'org-src-mode-hook 'tec-org-python)
(setq ess-eval-visibly 'nowait)
(setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
 (ess-R-fl-keyword:constants . t)
 (ess-R-fl-keyword:modifiers . t)
 (ess-R-fl-keyword:fun-defs . t)
 (ess-R-fl-keyword:assign-ops . t)
 (ess-R-fl-keyword:%op% . t)
 (ess-fl-keyword:fun-calls . t)
 (ess-fl-keyword:numbers . t)
 (ess-fl-keyword:operators . t)
 (ess-fl-keyword:delimiters . t)
 (ess-fl-keyword:= . t)
 (ess-R-fl-keyword:F&T . t)))
(add-hook 'LaTeX-mode-hook #'mixed-pitch-mode)
(setq tec/yas-latex-template-preamble "
\\usepackage[pdfa,unicode=true,hidelinks]{hyperref}

\\usepackage[dvipsnames,svgnames,table,hyperref]{xcolor}
\\renewcommand{\\UrlFont}{\\ttfamily\\small}

\\usepackage[a-2b]{pdfx} % why not be archival

\\usepackage[T1]{fontenc}
\\usepackage[osf,helvratio=0.9]{newpxtext} % pallatino
\\usepackage[scale=0.92]{sourcecodepro}

\\usepackage[varbb]{newpxmath}
\\usepackage{mathtools}
\\usepackage{amssymb}

\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}
% microtype makes text look nicer

\\usepackage{graphicx} % include graphics
\\usepackage{grffile} % fix allowed graphicx filenames

\\usepackage{booktabs} % nice table rules
")

(defun tec/yas-latex-get-class-choice ()
  "Prompt user for LaTeX class choice"
  (setq tec/yas-latex-class-choice (ivy-read "Select document class: " '("article" "scrartcl" "bmc") :def "bmc")))

(defun tec/yas-latex-preamble-if ()
  "Based on class choice prompt for insertion of default preamble"
    (if (equal tec/yas-latex-class-choice "bmc") 'nil
             (eq (read-char-choice "Include default preamble? [Type y/n]" '(?y ?n)) ?y)))
(setq TeX-fold-math-spec-list
      '(;; missing/better symbols
        ("≤" ("le"))
        ("≥" ("ge"))
        ("≠" ("ne"))
        ("★" ("star"))
        ;; conviniance shorts
        ("‹" ("left"))
        ("›" ("right"))
        ;; private macros
        ("ℝ" ("RR"))
        ("ℕ" ("NN"))
        ("ℤ" ("ZZ"))
        ("ℚ" ("QQ"))
        ("ℂ" ("CC"))
        ("ℙ" ("PP"))
        ("ℍ" ("HH"))
        ("𝔼" ("EE"))
        ("𝑑" ("dd"))
        ;; known commands
        ("" ("phantom"))
        ("({1}⧸{2})" ("frac"))
        ("‘{1}’" ("text"))
        ;; private commands
        ("|{1}|" ("abs"))
        ("‖{1}‖" ("norm"))
        ("⌊{1}⌋" ("floor"))
        ("⌈{1}⌉" ("ceil"))
        ("⌊{1}⌉" ("round"))
        ("⭡{1}" ("vec"))
        ("𝑑{1}⧸𝑑{2}" ("dv"))
        ("∂{1}⧸∂{2}" ("pdv"))
        )
      TeX-fold-macro-spec-list
      '(
        ;; as the defaults
        ("[f]" ("footnote" "marginpar"))
        ("[c]" ("cite"))
        ("[l]" ("label"))
        ("[r]" ("ref" "pageref" "eqref"))
        ("[i]" ("index" "glossary"))
        ("..." ("dots"))
        ("{1}" ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
            "textbf" "textsc" "textup"))
        ;; tweaked defaults
        ("©" ("copyright"))
        ("®" ("textregistered"))
        ("™"  ("texttrademark"))
        ("[1]:||►" ("item"))
        ("❡❡ {1}" ("part" "part*"))
        ("❡ {1}" ("chapter" "chapter*"))
        ("§ {1}" ("section" "section*"))
        ("§§ {1}" ("subsection" "subsection*"))
        ("§§§ {1}" ("subsubsection" "subsubsection*"))
        ("¶ {1}" ("paragraph" "paragraph*"))
        ("¶¶ {1}" ("subparagraph" "subparagraph*"))
        ;; extra
        ("⬖ {1}" ("begin"))
        ("⬗ {1}" ("end"))
        ))
(defvar +latex-use-TeX-fold t
  "Use TeX fold in TeX-mode.
When set to non-nil, this adds a few hooks/advices to fold stuff.")

;; Fold after cdlatex and snippets.
(defun +TeX-fold-line-ah (&rest _)
  "Auto-fold LaTeX macros after functions that typically insert them."
  (TeX-fold-region (line-beginning-position) (line-end-position)))

(when +latex-use-TeX-fold
  (advice-add #'cdlatex-math-symbol :after #'+TeX-fold-line-ah)
  (advice-add #'cdlatex-math-modify :after #'+TeX-fold-line-ah)
  ;; local after-snippet hook for folding
  (add-hook! 'TeX-mode-hook
    (add-hook 'yas-after-exit-snippet-hook #'+TeX-fold-line-ah nil t))
  ;; TeX-fold messes up the font face a bit too much, so
  (add-hook! 'mixed-pitch-mode-hook
    (when mixed-pitch-mode
      (let ((var-pitch (face-attribute 'variable-pitch :family))
            (var-height (face-attribute 'variable-pitch :height)))
        (add-to-list 'mixed-pitch-fixed-cookie
                     (face-remap-add-relative
                      'TeX-fold-folded-face :family var-pitch :height var-height))))))
(after! tex
  (map!
   :map LaTeX-mode-map
   :ei [C-return] #'LaTeX-insert-item

   ;; normal stuff here
   :localleader
   :desc "View" "v" #'TeX-view
   (:when +latex-use-TeX-fold
     :desc "Fold paragraph"     "f"   #'TeX-fold-paragraph
     :desc "Unfold paragraph"   "C-f" #'TeX-fold-clearout-paragraph
     :desc "Fold buffer"        "F"   #'TeX-fold-buffer
     :desc "Unfold buffer"      "C-F" #'TeX-fold-clearout-buffer))
  (setq TeX-electric-math '("\\(" . "")))
;; Making \( \) less visible
(defface unimportant-latex-face
  '((t
     :inherit font-lock-comment-face :family "Overpass" :weight light))
  "Face used to make \\(\\), \\[\\] less visible."
  :group 'LaTeX-math)

(font-lock-add-keywords
 'latex-mode
 `((,(rx (and "\\" (any "()[]"))) 0 'unimportant-latex-face prepend))
 'end)

(font-lock-add-keywords
 'latex-mode
 `((,"\\\\[[:word:]]+" 0 'font-lock-keyword-face prepend))
 'end)
(setq preview-LaTeX-command '("%`%l \"\\nonstopmode\\nofiles\
\\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %t \"}\""))
(after! cdlatex
  (setq ;; cdlatex-math-symbol-prefix ?\; ;; doesn't work at the moment :(
        cdlatex-math-symbol-alist
        '( ;; adding missing functions to 3rd level symbols
          (?_    ("\\downarrow"  ""           "\\inf"))
          (?^    ("\\uparrow"    ""           "\\sup"))
          (?k    ("\\kappa"      ""           "\\ker"))
          (?m    ("\\mu"         ""           "\\lim"))
          (?c    (""             "\\circ"     "\\cos"))
          (?d    ("\\delta"      "\\partial"  "\\dim"))
          (?D    ("\\Delta"      "\\nabla"    "\\deg"))
          ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
          (?F    ("\\Phi"))
          ;; now just conveniance
          (?.    ("\\cdot" "\\dots"))
          (?:    ("\\vdots" "\\ddots"))))
  cdlatex-math-modify-alist
  '( ;; my own stuff
    (?B    "\\mathbb"        nil          t    nil  nil)
    (?a    "\\abs"           nil          t    nil  nil)))
(after! ess-r-mode
  (appendq! +pretty-code-symbols
            '(:assign "⟵"
              :multiply "×"))
  (set-pretty-symbols! 'ess-r-mode
    ;; Functional
    :def "function"
    ;; Types
    :null "NULL"
    :true "TRUE"
    :false "FALSE"
    :int "int"
    :floar "float"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :in "%in%"
    :return "return"
    ;; Other
    :assign "<-"
    :multiply "%*%"))
(setq ledger-mode-should-check-version nil
      ledger-report-links-in-register nil
      ledger-binary-path "hledger")
(add-hook! (gfm-mode markdown-mode) #'mixed-pitch-mode)
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
(use-package! beancount
   :load-path "~/.config/doom/lisp"
   :mode ("\\.beancount\\'" . beancount-mode)
   :config (setq beancount-electric-currency t)
   ;; TODO make the following *work*
   :bind (:map beancount-mode-map ("S-RET" . #'beancount-align-to-previous-number)))
