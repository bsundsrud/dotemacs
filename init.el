;; Speed up startup
(defvar init--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)


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
(setq ido-auto-merge-delay-time 1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
(setq-default indent-tabs-mode nil)     ; use spaces instead of tabs
;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Use emacs terminfo
(setq system-uses-terminfo nil)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; UI settings
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

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
  :diminish
  :config (ivy-mode 1))

(use-package counsel
  :ensure t
  :diminish
  :config (counsel-mode 1))

(use-package swiper :ensure t)

(use-package diminish :ensure t)

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode 1))

(defun set-gopath-for-go-projects-hook ()
  "Projectile hook to detect a go project and run go-set-project."
  (if (eq major-mode 'go-mode)
      (go-set-project)))

(use-package projectile
  :ensure t
  :diminish
  :commands (projectile-mode)
  :init
  (add-hook 'projectile-after-switch-project-hook #'set-gopath-for-go-projects-hook)
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :diminish
  :commands (counsel-projectile-mode)
  :init
  (progn
    (projectile-mode t)
    (counsel-projectile-mode 1)))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (progn
    (setq editorconfig-indentation-alist
          (append editorconfig-indentation-alist '((vcl-mode c-basic-offset))))
    (editorconfig-mode 1)))

(use-package evil
  :ensure t
  :diminish 'undo-tree-mode
  :config
  (evil-mode 1)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "z") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode))

(use-package evil-goggles
  :ensure t
  :diminish
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package flycheck
  :diminish
  :ensure t
  :init (global-flycheck-mode 1))

(use-package eglot
  :ensure t
  :demand)

(use-package company
  :ensure t
  :diminish
  :bind ("C-<tab>" . company-complete)
  :config
  (global-company-mode 1))

(defun my-term-mode-hook ()
  "Disable line numbers in terminals, as that seems to screw with dimension calculation."
  (display-line-numbers-mode 0))

(use-package multi-term
  :ensure t
  :config
  (progn
    (setq multi-term-program "/bin/bash")
    (setq multi-term-dedicated-select-after-open-p t)
    (setq multi-term-dedicated-skip-other-window-p t)
    (add-hook 'term-mode-hook #'my-term-mode-hook)))

(use-package eterm-256color
  :ensure t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))
 
;; ================================================
;; UI Packages
;; ================================================
(use-package all-the-icons :ensure t)

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-emacs-theme))

;; ================================================
;; Language Support
;; ================================================

;; Rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'eglot-ensure)
  :config (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'toml-mode-hook 'cargo-minor-mode))

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode))

;; Org-mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode))

(use-package org-bullets
  :ensure t
  :diminish
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)

;; VCL
(use-package vcl-mode
  :mode "\\.vcl\\'"
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
(use-package docker-compose-mode
  :mode ("docker-compose.yml\\'"
         "docker-compose.yaml\\'")
  :ensure t)

;; Groovy/Jenkinsfile
(use-package groovy-mode
  :mode ("\\.groovy\\'"
         "Jenkinsfile\\'")
  :ensure t)

;; Web
(use-package web-mode
  :mode "\\.html\\'"
  :ensure t)

;; JavaScript
;; eglot support requires running `npm install --global javascript-typescript-langserver` first
(use-package js2-mode
  :mode "\\.js\\'"
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'eglot-ensure))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

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

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun last-term-buffer (l)
  "Return most recently used term buffer from buffer list L."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

;; Override emacs globals
(general-define-key
 "C-s" 'swiper
 "M-x" 'counsel-M-x
 "C-x C-b" 'ibuffer
 "M-s-u" 'revert-buffer-no-confirm
 "<f12>" 'multi-term-dedicated-toggle)

;; Code actions via eglot
(general-define-key
 "C-c r" 'eglot-rename
 "C-c f" 'eglot-format
 "C-c C-f" 'eglot-format-buffer
 "C-c h" 'eglot-help-at-point
 "C-c TAB" 'eglot-code-actions
 )

;; setup normal mode evil shortcuts
(general-define-key
 :states '(normal)
 "gT"  '(switch-to-prev-buffer :which-key "previous buffer")
 "gt"  '(switch-to-next-buffer :which-key "next buffer")
 "gb"  '(counsel-ibuffer :which-key "buffers list")
 "`" '(neotree-toggle :which-key "toggle neotree"))

;; setup evil leader shortcuts
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "/"   '(counsel-rg :which-key "ripgrep")
 "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "RET" 'get-term
 "g"   '(:ignore t :which-key "buffers")
 "gT"  '(switch-to-prev-buffer :which-key "previous buffer")
 "gt"  '(switch-to-next-buffer :which-key "next buffer")
 "gb"  '(counsel-ibuffer :which-key "buffers list")
 "gq"  '(kill-current-buffer :which-key "kill this buffer")
 "w"   '(:ignore t :which-key "window")
 "wl"  '(windmove-right :which-key "move right")
 "wh"  '(windmove-left :which-key "move left")
 "wk"  '(windmove-up :which-key "move up")
 "wj"  '(windmove-down :which-key "move down")
 "w/"  '(split-window-right :which-key "split right")
 "w-"  '(split-window-below :which-key "split below")
 "wx"  '(delete-window :which-key "delete window")
 "q"   '(:ignore t :which-key "quit")
 "qz"  '(delete-frame :which-key "kill frame")
 "qQ"  '(kill-emacs :which-key "kill emacs")
 "qq"  '(save-buffers-kill-emacs :which-key "save and kill emacs")
 "e"   '(:ignore t :which-key "elisp eval")
 "eb"  '(eval-buffer :which-key "current buffer")
 "t"   '(:ignore t :which-key "text properties")
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
    (groovy-mode racket-mode docker-compose-mode multiline multi-term vcl-mode flymake eglot company-go evil-goggles go-mode web-mode flycheck-rust cargo dockerfile-mode toml-mode yaml-mode org-bullets counsel-projectile projectile spaceline-all-the-icons diminish lsp-rust rust-mode spaceline company lsp-ui lsp-mode flycheck doom-themes evil neotree all-the-icons which-key counsel ivy general use-package)))
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-flycheck-alternate t)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(spaceline-all-the-icons-separators-invert-direction t)
 '(spaceline-all-the-icons-slim-render t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

;; Restore settings
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist init--file-name-handler-alist)
(put 'erase-buffer 'disabled nil)
