;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3))

(after! web-mode
(after! tide
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'web-mode-hook
            (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode))
)
(setq exec-path (append exec-path '("~/.nvm/versions/node/v10.15.3/bin")))
(map! :leader
      :prefix ("o" . "open")
       :when (featurep! :tools vterm)
       :desc "Terminal"          "T" #'+vterm/open
       :desc "Terminal in popup" "t" #'+vterm/open-popup-in-project)
