;; Usability stuff
(setq display-line-numbers-type 'relative)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Custom bindings
(map! :leader
      :prefix ("o" . "open")
       :when (featurep! :tools vterm)
       :desc "Terminal"          "T" #'+vterm/open
       :desc "Terminal in popup" "t" #'+vterm/open-popup-in-project)

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

;; Node executable path
(setq exec-path (append exec-path '("~/.nvm/versions/node/v10.15.3/bin")))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3))

(require 'ox-publish)
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

;; Org capture templates
(after! org-capture
  (add-to-list 'org-capture-templates
               '("s" "Song" entry (file+headline "~/org/bookmarks.org" "Music")
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
