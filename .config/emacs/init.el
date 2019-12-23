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
(straight-use-package 'evil)
(straight-use-package 'flycheck)
(straight-use-package 'company)
(straight-use-package 'lsp-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'cargo)
(straight-use-package 'flycheck-rust)
(straight-use-package 'lsp-ui)
(straight-use-package 'yasnippet)
(straight-use-package 'company-lsp)
(straight-use-package 'smartparens)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'projectile)
(straight-use-package 'helm)
(straight-use-package 'use-package-hydra)
(straight-use-package 'ace-window)
(straight-use-package 'hydra)
(straight-use-package 'helm-projectile)
(straight-use-package 'one-themes)
(straight-use-package 'nix-mode)
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'web-mode)
(straight-use-package 'impatient-mode)
(straight-use-package 'tide)

(use-package treemacs
  :defer t)

(use-package treemacs-projectile
  :after treemacs prijectile)

(use-package treemacs-evil
  :after treemacs evil)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package one-themes
  :init (load-theme 'one-dark t))

(use-package use-package-hydra)

(use-package hydra)

(use-package helm
  :config (require 'helm-config)
  :bind ("M-x" . helm-M-x))

(use-package ace-window
  :after hydra)

(use-package yasnippet-snippets)

(use-package smartparens
  :config
    (require 'smartparens-config)
    (smartparens-global-mode t))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package evil
  :after hydra
  :config (evil-mode 1)
          (define-key evil-normal-state-map (kbd "SPC") 'hydra-leader/body)
  :hydra (hydra-leader (:color blue)
  ("w" ace-window "Switch window")
  ("p" hydra-project/body "Projects")
  ("b" hydra-buffer/body "Buffer")
  ("f" hydra-file/body "File")
  ("t" treemacs "Open Treemacs")
  ("<SPC>" helm-projectile-find-file "Open file in project")
  ("i" (find-file "~/.config/emacs/init.el") "Open init.el")))

(use-package flycheck
  :diminish
  :init (global-flycheck-mode)
  :custom (flycheck-display-errors-delay .3)
  :hook (prog-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :commands lsp
  :config (add-hook 'rust-mode-hook #'lsp))

(use-package company-lsp
  :after company lsp-mode
  :config
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0.3)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
  :init
    (push 'company-lsp company-backends))

(use-package toml-mode)

(use-package rust-mode
  :hook
    (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package lsp-ui)

(use-package helm-projectile
  :after projectile
  :hydra (hydra-project (:color blue)
			("p" helm-projectile-switch-project "Open project")
			("a" projectile-add-known-project "Add project")))

(use-package web-mode
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
  :commands (impatient-mode))


(use-package projectile
  :after hydra
  :config (projectile-mode +1)
          (setq projectile-project-search-path '("/mnt/hdd/projects")))

(defhydra hydra-buffer (:color blue)
                       ("b" helm-buffers-list "List buffers")
                       ("e" eval-buffer "Eval buffer")
                       ("k" kill-current-buffer "Kill current buffer"))

(defhydra hydra-file (:color blue)
                     ("s" save-buffer "Save file"))

(menu-bar-mode -1)
(tool-bar-mode -1)
