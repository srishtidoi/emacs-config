;; [[file:~/.config/doom/config.org::*Package loading][Package loading:1]]
;; -*- no-byte-compile: t; -*-
;; Package loading:1 ends here

;; [[file:~/.config/doom/config.org::*Auto-complete][Auto-complete:1]]
(package! company-tabnine ; tab9 autocomplete
  :recipe (:host github :repo "TommyX12/company-tabnine"
           :files ("company-tabnine.el" "fetch-binaries.sh")))
;; Auto-complete:1 ends here

;; [[file:~/.config/doom/config.org::*Prettification][Prettification:1]]
(package! prettify-utils ; simplify messing with prettify-mode
  :recipe (:host github :repo "Ilazki/prettify-utils.el"))
;; Prettification:1 ends here

;; [[file:~/.config/doom/config.org::*Window management][Window management:1]]
(package! rotate)
;; Window management:1 ends here

;; [[file:~/.config/doom/config.org::*Fun][Fun:1]]
(package! xkcd)
;; Fun:1 ends here

;; [[file:~/.config/doom/config.org::*Fun][Fun:2]]
(package! selectric-mode)
;; Fun:2 ends here

;; [[file:~/.config/doom/config.org::*Fun][Fun:3]]
(package! wttrin)
;; Fun:3 ends here

;; [[file:~/.config/doom/config.org::*Fun][Fun:4]]
(package! spray)
;; Fun:4 ends here

;; [[file:~/.config/doom/config.org::*Fun][Fun:5]]
(package! theme-magic)
;; Fun:5 ends here

;; [[file:~/.config/doom/config.org::*Flyspell-lazy][Flyspell-lazy:1]]
(package! flyspell-lazy)
;; Flyspell-lazy:1 ends here

;; [[file:~/.config/doom/config.org::*CalcTeX][CalcTeX:1]]
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                                 :files ("*.el")))
;; CalcTeX:1 ends here

;; [[file:~/.config/doom/config.org::*ESS][ESS:1]]
(package! ess-view)
;; ESS:1 ends here

;; [[file:~/.config/doom/config.org::*Systemd][Systemd:1]]
(package! systemd)
;; Systemd:1 ends here

;; [[file:~/.config/doom/config.org::*Org Mode][Org Mode:1]]
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table"))
;; Org Mode:1 ends here

;; [[file:~/.config/doom/config.org::*Org Mode][Org Mode:2]]
(package! ox-gfm)
;; Org Mode:2 ends here

;; [[file:~/.config/doom/config.org::*Org Mode][Org Mode:3]]
(package! org-ref)
;; Org Mode:3 ends here

;; [[file:~/.config/doom/config.org::*Org Mode][Org Mode:4]]
(package! org-fragtog)
;; Org Mode:4 ends here

;; [[file:~/.config/doom/config.org::*Org Mode][Org Mode:5]]
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))
;; Org Mode:5 ends here

;; [[file:~/.config/doom/config.org::*Org Mode][Org Mode:6]]
(package! org-chef)
;; Org Mode:6 ends here
