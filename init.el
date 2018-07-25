;; ================================================
;; Global prefs
;; ================================================
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";; Scratch ELisp Buffer") ; print a default message in the empty scratch buffer opened at startup
(setq show-paren-delay 0)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; UI settings
(add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))

;; ================================================
;; Packaging
;; ================================================
(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

;; ================================================
;; Core packages
;; ================================================
(use-package avy :ensure t)

(use-package general :ensure t)

(use-package ivy
  :ensure t
  :config (ivy-mode 1))

(use-package counsel
  :ensure t
  :config (counsel-mode 1))

(use-package swiper :ensure t)

(use-package diminish :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :config (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :commands (counsel-projectile-mode)
  :init
  (progn
    (projectile-mode t)
    (counsel-projectile-mode 1)))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode 1))

(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

(use-package company
  :ensure t
  :bind ("C-<tab>" . company-complete)
  :config
  (global-company-mode 1))
 
;; ================================================
;; UI Packages
;; ================================================
(use-package all-the-icons :ensure t)

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme))

;; ================================================
;; Language Support
;; ================================================

;; Rust
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package lsp-rust
  :ensure t
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-mode))

;; Org-mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)

;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

;; YAML
(use-package yaml-mode
  :mode ("\\.yaml\\'"
	 "\\.yml\\'")
  :ensure t)

;; TOML
(use-package toml-mode
  :mode "\\.toml\\'"
  :ensure t)

;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t)

;; ================================================
;; Theme
;; ================================================

(use-package doom-themes
  :ensure t
  :config
  (progn
    (setq doom-one-brighter-comments t)
    (load-theme 'doom-one t)))

;; ================================================
;; Keybindings
;; ================================================

;; Override emacs globals
(general-define-key
 "C-s" 'swiper
 "M-x" 'counsel-M-x
 )

;; setup normal mode evil shortcuts
(general-define-key
 :states '(normal)
 "`" '(neotree-toggle :which-key "toggle neotree"))

;; setup evil leader shortcuts
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "/" '(counsel-rg :which-key "ripgrep")
 "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "w" '(:ignore t :which-key "window")
 "wl" '(windmove-right :which-key "move right")
 "wh" '(windmove-left :which-key "move left")
 "wk" '(windmove-up :which-key "move up")
 "wj" '(windmove-down :which-key "move down")
 "w/" '(split-window-right :which-key "split right")
 "w-" '(split-window-below :which-key "split below")
 "wx" '(delete-window :which-key "delete window")
 "q" '(:ignore t :which-key "quit")
 "qz" '(delete-frame :which-key "kill frame")
 "qQ" '(kill-emacs :which-key "kill emacs")
 "qq" '(save-buffers-kill-emacs :which-key "save and kill emacs")
 "e" '(:ignore t :which-key "elisp eval")
 "eb" '(eval-buffer :which-key "current buffer")
 "t" '(:ignore t :which-key "text properties")
 "ti"  '(text-scale-increase :which-key "text scale increase")
 "td"  '(text-scale-decrease :which-key "text scale decrease")
 )

;; ================================================
;; Start server if not running
;; ================================================
(use-package server
  :config
  (progn
    (defun server-enable ()
      (unless (server-running-p)
        (server-start)))
    (add-hook 'after-init-hook 'server-enable t)))

;; ================================================
;; Autogenerated
;; ================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode toml-mode yaml-mode org-bullets counsel-projectile projectile spaceline-all-the-icons diminish lsp-rust rust-mode spaceline company lsp-ui lsp-mode flycheck doom-themes evil neotree all-the-icons which-key counsel ivy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )