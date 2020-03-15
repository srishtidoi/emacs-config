;; -*- no-byte-compile: t; -*-

(package! company-tabnine ; tab9 autocomplete
  :recipe (:host github :repo "TommyX12/company-tabnine"
           :files ("company-tabnine.el" "fetch-binaries.sh")))

(package! prettify-utils ; simplify messing with prettify-mode
  :recipe (:host github :repo "Ilazki/prettify-utils.el"))

(package! rotate)

(package! xkcd)

(package! selectric-mode)

(package! wttrin)

(package! spray)

(package! theme-magic)

(package! flyspell-lazy)

(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                                 :files ("*.el")))

(package! ess-view)

(package! systemd)

(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table"))

(package! ox-gfm)

(package! org-ref)

(package! org-fragtog)

(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))

(package! org-chef)
