;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*Prettification][Prettification:1]]
(package! prettify-utils ; simplify messing with prettify-mode
  :recipe (:host github :repo "Ilazki/prettify-utils.el") :pin "8b783d316c23bffdf2b9e6f52635482c4812ee43")
;; Prettification:1 ends here

;; [[file:config.org::*Window management][Window management:1]]
(package! rotate :pin "091b5ac4fc310773253efb317e3dbe8e46959ba6")
;; Window management:1 ends here

;; [[file:config.org::*Fun][Fun:1]]
(package! xkcd :pin "66e928706fd660cfdab204c98a347b49c4267bdf")
;; Fun:1 ends here

;; [[file:config.org::*Fun][Fun:2]]
(package! selectric-mode :pin "bb9e66678f34e9bc23624ff6292cf5e7857e8e5f")
;; Fun:2 ends here

;; [[file:config.org::*Fun][Fun:3]]
(package! wttrin :recipe (:local-repo "lisp" :no-byte-compile t))
;; Fun:3 ends here

;; [[file:config.org::*Fun][Fun:4]]
(package! spray :pin "00638bc916227f2f961013543d10e85a43a32e29")
;; Fun:4 ends here

;; [[file:config.org::*Fun][Fun:5]]
(package! theme-magic :pin "844c4311bd26ebafd4b6a1d72ddcc65d87f074e3")
;; Fun:5 ends here

;; [[file:config.org::*Fun][Fun:6]]
(package! elcord :pin "01b26d1af2f33a7c7c5a1c24d8bfb6d40115a7b0")
;; Fun:6 ends here

;; [[file:config.org::*Fun][Fun:7]]
(package! keycast :pin "038475c178e90c7bad64d113db26d42cad60e149")
;; Fun:7 ends here

;; [[file:config.org::*Flyspell-lazy][Flyspell-lazy:1]]
(package! flyspell-lazy :pin "3ebf68cc9eb10c972a2de8d7861cbabbbce69570")
;; Flyspell-lazy:1 ends here

;; [[file:config.org::*CalcTeX][CalcTeX:1]]
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                                 :files ("*.el")) :pin "7fa2673c64e259e04aef684ccf09ef85570c388b")
;; CalcTeX:1 ends here

;; [[file:config.org::*ESS][ESS:1]]
(package! ess-view :pin "d4e5a340b7bcc58c434867b97923094bd0680283")
;; ESS:1 ends here

;; [[file:config.org::*Magit Delta][Magit Delta:1]]
;; (package! magit-delta :recipe (:host github :repo "dandavison/magit-delta") :pin "0c7d8b2359")
;; Magit Delta:1 ends here

;; [[file:config.org::*Info colours][Info colours:1]]
(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")
;; Info colours:1 ends here

;; [[file:config.org::*Email][Email:1]]
(package! org-msg)
;; Email:1 ends here

;; [[file:config.org::*Email][Email:2]]
(package! mu4e-alert)
;; Email:2 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
(package! evil-tex :recipe (:host github :repo "itai33/evil-tex")
                            :pin "925337a4d5799d48c03e0384757decb6363ab478")
;; LaTeX:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:2]]
(package! auto-latex-snippets
  :recipe (:local-repo "~/Desktop/TEC/Programming/GitHub/Auto-LaTeX-Snippets"
           :no-byte-compile t))
;; LaTeX:2 ends here

;; [[file:config.org::*Org Mode][Org Mode:1]]
(unpin! org)
;; Org Mode:1 ends here

;; [[file:config.org::*Improve agenda/capture][Improve agenda/capture:1]]
(package! org-super-agenda :pin "dd0d104c269fab9ebe5af7009bc1dd2a3a8f3c12")
;; Improve agenda/capture:1 ends here

;; [[file:config.org::*Improve agenda/capture][Improve agenda/capture:2]]
(package! doct
  :recipe (:host github :repo "progfolio/doct")
  :pin "80d291e5f1cbdabd4eb7f88c917653c59d3f14be")
;; Improve agenda/capture:2 ends here

;; [[file:config.org::*Visuals][Visuals:1]]
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "88380f865a79bba49e4f501b7fe73a7bfb03bd1a")
;; Visuals:1 ends here

;; [[file:config.org::*Visuals][Visuals:2]]
(package! org-fragtog :pin "92119e3ae7c9a0ae2b5c9d9e4801b5fdc4804ad7")
;; Visuals:2 ends here

;; [[file:config.org::*Visuals][Visuals:3]]
(package! org-pretty-tags :pin "40fd72f3e701e31813f383fb429d30bb88cee769")
;; Visuals:3 ends here

;; [[file:config.org::*Extra functionality][Extra functionality:1]]
(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
;; Extra functionality:1 ends here

;; [[file:config.org::*Extra functionality][Extra functionality:2]]
(package! org-ref :pin "9a8053f0b0e799b8edbae2376dda44704054725a")
;; Extra functionality:2 ends here

;; [[file:config.org::*Extra functionality][Extra functionality:3]]
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d70d2c19511efccc491bed3ca0758170")
;; Extra functionality:3 ends here

;; [[file:config.org::*Extra functionality][Extra functionality:4]]
(package! org-chef :pin "77f97ad07b957d6b5ea6c2a57a9e623f2ab36d04")
;; Extra functionality:4 ends here

;; [[file:config.org::*Extra functionality][Extra functionality:5]]
;; (package! org-plot :recipe (:local-repo "lisp" :no-byte-compile t))
;; Extra functionality:5 ends here

;; [[file:config.org::*Extra functionality][Extra functionality:6]]
(package! org-roam-server :pin "bfc70327413948fb7e631f4424fb92c3dbdeafc0")
;; Extra functionality:6 ends here

;; [[file:config.org::*Systemd][Systemd:1]]
(package! systemd :pin "51c148e09a129ddf33d95276aa0e89d4ef6f8dd2")
;; Systemd:1 ends here
