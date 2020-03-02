;;; init --- My init file! :)

;; Author: Rodrigo Leite <rodrigo@leite.dev>
;; Version: 1.3
;; Keywords: evil, helm, projectile, hydra, leader
;; URL: http://github.com/RodrigoLeiteF/dotfiles

;;; Commentary:

;; This package is my init file.  It's a mess at the moment, sorry

;;; Code:

; Install straight.el so we can use packages!
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; List packages that we need
(straight-use-package 'use-package)

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-unknown 'fa_question_circle)

  (setq company-box-icons-elisp
        '((fa_tag :face font-lock-function-name-face) ;; Function
          (fa_cog :face font-lock-variable-name-face) ;; Variable
          (fa_cube :face font-lock-constant-face) ;; Feature
          (md_color_lens :face font-lock-doc-face))) ;; Face

  (setq company-box-icons-yasnippet 'fa_bookmark)

        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
                (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
                (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
                (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.1))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
                (Template . ,(all-the-icons-material "format_align_left" :height 0.85 :v-adjust -0.2)))
              company-box-icons-alist 'company-box-icons-all-the-icons)
)

(use-package all-the-icons
  :straight t)

(use-package exec-path-from-shell
  :straight t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("NVM_BIN" "NPMBIN" "LC_ALL" "LANG" "LC_TYPE"
     "SSH_AGENT_PID" "SSH_AUTH_SOCK" "SHELL")))

(use-package eyebrowse
  :straight t
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "C-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "C-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "C-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "C-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

(use-package evil-commentary
  :straight t
  :config (evil-commentary-mode))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

(use-package f
  :straight t)

(use-package typescript-mode
  :straight t)

(use-package elm-mode
  :straight t
  :init (add-hook 'elm-mode-hook #'elm-format-on-save-mode))

(use-package use-package-hydra
  :straight t)

(use-package ivy
  :straight t
  :hook (after-init . counsel-mode))

(use-package linum-relative
  :straight t
  :config (linum-relative-global-mode))

(use-package tramp
  :straight t
  :config (add-to-list 'tramp-methods
                       '("yadm"
                         (tramp-login-program "yadm")
                         (tramp-login-args (("enter")))
                         (tramp-login-env (("SHELL") ("/bin/sh")))
                         (tramp-remote-shell "/bin/sh")
                         (tramp-remote-shell-args ("-c"))))
          (setenv "SHELL" "/bin/bash"))

(use-package magit
  :after use-package-hydra hydra
  :straight t
  :hydra (hydra-magit (:color blue)
		      ("g" magit "Open magit window")
		      ("d" (magit-status "/yadm::") "Open magit window for YADM")))

(use-package evil-magit
  :after magit
  :straight t)

(use-package lsp-ivy
  :straight (lsp-ivy :type git :host github :repo "emacs-lsp/lsp-ivy"
                      :fork (:host github
                             :repo "RodrigoLeiteF/lsp-ivy")))
(use-package ripgrep
  :straight t)

(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package treemacs
  :straight t
  :defer t
  :config (setq treemacs-is-never-other-window nil))

(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

(use-package treemacs-evil
  :straight t
  :after treemacs evil)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package one-themes
  :straight t
  :init (load-theme 'one-dark t))

(use-package ace-window
  :straight t
  :after hydra)

(use-package yasnippet-snippets
  :straight t)

(use-package smartparens
  :straight t
  :config
    (require 'smartparens-config)
    (smartparens-global-mode t))

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package evil
  :straight t
  :after hydra
  :config (evil-mode 1)
          (define-key evil-normal-state-map (kbd "SPC") 'hydra-leader/body)
          (define-key evil-insert-state-map (kbd "<C-SPC>") 'company-complete)
          (define-key evil-normal-state-map (kbd "<C-SPC>") 'company-complete)
	  (define-key evil-normal-state-map (kbd "s") #'evil-avy-goto-line)
	  (define-key evil-normal-state-map (kbd ";") #'evil-avy-goto-char)

  :init (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)

  :hydra (hydra-leader (:color blue)
  ("w" ace-window "Switch window")
  ("p" hydra-project/body "Projects")
  ("b" hydra-buffer/body "Buffer")
  ("f" hydra-file/body "File")
  ("t" treemacs "Open Treemacs")
  ("m" counsel-M-x "Open M-x")
  ("l" hydra-lsp/body "LSP Actions")
  ("g" hydra-magit/body "Magit menu")
  ("e" hydra-error/body "Errors menu")
  ("<SPC>" counsel-projectile-find-file "Open file in project")
  ("i" hydra-insert/body "Insert")
  ("o" hydra-org/body "Org mode")
  ("n" (find-file "~/.config/emacs/init.el") "Open init.el")))

(use-package org
  :straight t
  :config
  (setq org-capture-templates
      '(("i" "Idea" entry (file+headline "~/org/ideas.org" "Idea")
         "* TODO %^{Type|Post|Project}\n  %i\n  %t")
        )
  )
)

(use-package flycheck
  :straight t
  :diminish
  :init (global-flycheck-mode)
  :custom (flycheck-display-errors-delay .3)
  :hook (prog-mode . flycheck-mode))

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
    (setq company-tooltip-align-annotations t)
    (setq company-minimum-prefix-length 1)
    ;; (push 'company-yasnippet company-backends)
    )

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-prefer-flymake nil)
    (add-hook 'before-save-hook #'lsp-format-buffer)
  :hook
    (prog-mode . lsp)
  :hydra (hydra-lsp (:color blue)
		    ("a" lsp-ivy-execute-code-action "Execute code action")
		    ("r" lsp-rename "Rename symbol")
		    ("d" lsp-describe-session "Describe session")
		    ("o" lsp-organize-imports "Organize imports")
		    ("x" lsp-workspace-restart "Restart workspace")))

(use-package company-lsp
  :straight t
  :after company lsp-mode
  :config
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0.3)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
  :init
    (push 'company-lsp company-backends))

(use-package toml-mode
  :straight t)

(use-package rustic
  :straight t
  :config (setq rustic-lsp-server nil))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :straight t)

(use-package lsp-ui
  :straight t)

(use-package counsel-projectile
  :straight t
  :after projectile
  :hydra (hydra-project (:color blue)
			("p" counsel-projectile-switch-project "Open project")
			("a" projectile-add-known-project "Add project")
			("r" projectile-remove-known-project "Remove project")
			("f" projectile-ripgrep "Find in project")))

(use-package web-mode
  :straight t
  :mode (("\\.html$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :hook ((web-mode . company-mode)
         (web-mode . (lambda ()
                       (highlight-indent-guides-mode -1))))
  :config

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 2)

  (with-eval-after-load "smartparens"
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" nil :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair) "\\(?:>\\|]\\|}\\)+")))))
    (setf (alist-get nil web-mode-engines-auto-pairs) nil))

  ;; highlight matching tag
  (setq web-mode-enable-current-element-highlight t)

  (defun my-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;; enable flycheck
    (flycheck-mode)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))

    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  (defun my-web-mode-hook ()
    "company hook for `web-mode' for non-html buffers."
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (defun my-lsp-html-mode-hook ()
    " company hook for `web-mode' for html buffers."
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (defun lsp-html-setup ()
    "Function to setup `lsp-html'"
    (lsp)
    (my-lsp-html-mode-hook)
    (emmet-mode)
    (setq-local lsp-highlight-symbol-at-point nil)
    (bind-key "C-c o b" #'browse-url-of-file (current-local-map)))

  (add-hook 'web-mode-hook
            (lambda ()
              (pcase (file-name-extension buffer-file-name)
                ("tsx" (my-tide-setup-hook))
                ("html" (lsp-html-setup))
                (_ (my-web-mode-hook)))))

  ;; colorize colors in buffers
  (setq web-mode-enable-css-colorization t))

;; impatient mode: Live refresh of web pages
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :straight t
  :commands (impatient-mode))


(use-package projectile
  :straight t
  :after hydra
  :config (projectile-mode +1)
          (setq projectile-project-search-path '("/mnt/hdd/projects")))

(use-package ivy-posframe
  :after ivy
  :straight t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1)
  )

(use-package org-projectile
  :straight t
  :config
   (org-projectile-per-project)
   (setq org-projectile-per-project-filepath "todos.org")
   (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
   (add-to-list 'org-capture-templates
                (org-projectile-project-todo-entry))
  )

(use-package org-evil
  :straight t)

(defhydra hydra-buffer (:color blue)
                       ("b" ivy-switch-buffer "List buffers")
                       ("e" eval-buffer "Eval buffer")
                       ("f" lsp-format-buffer "Format buffer")
                       ("k" kill-current-buffer "Kill current buffer"))

(defhydra hydra-file (:color blue)
		     ("f" counsel-find-file "Find file")
		     ("s" save-buffer "Save file"))

(defhydra hydra-lsp-find (:color blue)
		    ("d" lsp-find-definition "Find definition")
		    ("r" lsp-find-references "Find references")
		    ("i" lsp-find-implementation "Find implementation")
		    ("t" lsp-find-type-definition "Find type definition")
		    ("c" lsp-find-declaration "Find declaration"))

(defhydra hydra-insert (:color blue)
  ("t" org-insert-todo-heading-respect-content "Todo")
  ("h" org-insert-heading-after-current "Heading")
  ("s" yas-insert-snippet "Snippet"))

(defhydra hydra-org (:color blue)
  ("c" org-capture "Capture")
  ("a" org-agenda "Agenda")
  ("t" org-todo "Cycle todo"))

(defhydra hydra-error (:color blue)
		    ("e" flycheck-list-errors "List errors")
		    ("j" flycheck-next-error "Next error")
		    ("k" flycheck-previous-error "Previous error"))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)

(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
