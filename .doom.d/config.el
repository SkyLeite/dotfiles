;; Usability stuff
(setq display-line-numbers-type 'relative)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(load-theme 'doom-nord t)

;; Custom bindings
(map! :leader
      :prefix ("o" . "open")
       :when (featurep! :tools vterm)
       :desc "Terminal"          "T" #'+vterm/open
       :desc "Terminal in popup" "t" #'+vterm/open-popup-in-project)

;; (after! web-mode
;; (after! tide
;;     (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;     (add-hook 'web-mode-hook
;;             (lambda ()
;;                 (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode))))
;;     ;; enable typescript-tslint checker
;;     (flycheck-add-mode 'typescript-tslint 'web-mode))
;; )

;; Node executable path
(setq exec-path (append exec-path '("~/.nvm/versions/node/v10.15.3/bin")))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3))

(after! ox
  (require 'ox-hugo))
(setq org-publish-project-alist
      '(
            ("org-notes"
                :base-directory "~/Repos/blog"
                :base-extension "org"
                :publishing-directory "~/public_html/"
                :recursive t
                :publishing-function org-html-publish-to-html
                :headline-levels 4             ; Just the default for this project.
                :auto-preamble t
            )

            ("org-static"
                :base-directory "~/Repos/blog"
                :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                :publishing-directory "~/public_html/"
                :recursive t
                :publishing-function org-publish-attachment
            )

            ("org" :components ("org-notes" "org-static"))
      ))

;; Rust stuff
(setq rust-format-on-save t)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

;; Org capture templates
(after! org-capture
  (add-to-list 'org-capture-templates
               '("s" "Song" entry (file+headline "~/org/bookmarks.org" "Music")
                 "* TODO %x"))
  (add-to-list 'org-capture-templates
               '("t" "TODO" entry (file+headline "~/Documents/agenda.org" "Not filed")
                 "* TODO %x"))
  )

(add-hook 'org-babel-pre-tangle-hook
          (lambda () (org-macro-replace-all (org-macro--collect-macros))))

(defun org-tangle-without-saving ()
  (interactive)
  (cl-letf (((symbol-function 'save-buffer) #'ignore))
    (org-babel-tangle (buffer-file-name))
  )
  (undo-tree-undo))

(after! org
  (setq org-babel-js-function-wrapper
        "require('sys').print(require('sys').inspect(function(){\n%s\n}()));"))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;; (set-face-attribute 'default nil :height 130)
(add-to-list 'exec-path "/home/rodrigo/.npm/bin")

;; Haskell
(setq lsp-haskell-process-path-hie "hie-wrapper")

;; Elixir
(setq lsp-clients-elixir-server-executable "elixir-ls")
(setq auth-sources '("~/.authinfo"))

(after! org-gcal
  (setq org-gcal-client-id "460989211468-4v46gg3v3c9fvs33md69vpsdd3u6vlrh.apps.googleusercontent.com"
      org-gcal-client-secret "LsFJC_x_OPokYcfwBkNPy6N6"
      org-gcal-file-alist '(("rodrigolf.dev@gmail.com" .  "~/Documents/agenda.org")
                            ("it1uheh885l77iuj1pilhm8skg@group.calendar.google.com" .  "~/Documents/work.org"))))

(after! company-mode
  (add-hook 'after-init-hook 'company-statistics-mode))
