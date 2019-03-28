;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3))

(after! tide
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))

(map! :leader
      :prefix ("o" . "open")
       :when (featurep! :tools vterm)
       :desc "Terminal"          "T" #'+vterm/open
       :desc "Terminal in popup" "t" #'+vterm/open-popup-in-project)
