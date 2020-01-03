;; -*- no-byte-compile: t; -*-
(package! company-tabnine ; tab9 autocomplete
  :recipe (:host github :repo "TommyX12/company-tabnine"
           :files ("company-tabnine.el" "fetch-binaries.sh")))
(package! prettify-utils ; simplify messing with prettify-mode
  :recipe (:host github :repo "Ilazki/prettify-utils.el"))
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! ox-gfm)
(package! ox-pandoc)
