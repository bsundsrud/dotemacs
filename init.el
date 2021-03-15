;;; init.el --- Emacs main configuration file -*- lexical-binding: t; no-byte-compile: t -*-
;;;
;;; Commentary:
;;; Emacs configuration.
;;;
;;; Code:

;; System probing
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst AT_LEAST_EMACS27 (not (version< emacs-version "27.0")))

;; If not emacs27, then manually run early-init.el
(unless AT_LEAST_EMACS27
  (message "Manually invoking early-init")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

;; ================================================
;; Packaging Init
;; ================================================
;; Load local packages
(let ((default-directory  (concat user-emacs-directory (convert-standard-filename "packages/"))))
  (normal-top-level-add-subdirs-to-load-path))

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


;; (defvar package-archives)
;; (setq package-archives '(("org"       . "http://orgmode.org/elpa/")
;;                          ("gnu"       . "http://elpa.gnu.org/packages/")
;;                          ("melpa"     . "https://melpa.org/packages/")))
;; (package-initialize)

;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package) ; unless it is already installed
;;   (package-refresh-contents) ; updage packages archive
;;   (package-install 'use-package)) ; and install the most recent version of use-package

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure nil))



;; ================================================
;; Global prefs
;; ================================================
(setq user-mail-address "benn.sundsrud@gmail.com"
      user-full-name "Benn Sundsrud")
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.emacs.d/emacs-backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/emacs-backups" t)))
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq version-control t )		; use version control
(setq vc-follow-symlinks t )	        ; don't ask for confirmation when opening symlinked file
(setq-default mouse-wheel-progressive-speed nil
              auto-window-vscroll nil)  ; change scroll behavior to be actually predictable

(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; Clipboard contents could be non-utf8 on windows
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))
(savehist-mode 1)                       ; save history between sessions
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; delete trailing whitespace
(setq-default bidi-display-reordering 'left-to-right) ; disable RTL for a performance boost
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "") ; set scratch to be text instead of elisp and blank
(delete-selection-mode t)               ; Overwrite selection by typing
(setq show-paren-delay 0)               ; Don't delay before highlighting matching parens
(show-paren-mode 1)                     ; enable show-paren-mode globally
(setq-default indent-tabs-mode nil)     ; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ; Answer y or n instead of having to type yes or no
(put 'erase-buffer 'disabled nil)       ; enable the erase-buffer command
(global-so-long-mode 1)                 ; enable so-long mode for better long line handling
(setq split-height-threshold nil)
(setq split-width-threshold 80)
;; Minimal UI
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode   -1))
(tooltip-mode    -1)
(menu-bar-mode   -1)
(fset 'menu-bar-open nil)

;; use bar cursor
(when window-system
  (setq-default cursor-type 'bar
                cursor-in-non-selected-windows nil))

;; Name the frame after the current buffer name
(setq-default frame-title-format '("%b — Emacs"))

;; Use emacs terminfo
(setq system-uses-terminfo nil)

;; Fancy titlebar for MacOS
(when IS-MAC
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

;; UI settings
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
;; ================================================
;; Core packages
;; ================================================

;; GC magic hack
(use-package gcmh
  :straight t
  :commands gcmh-mode
  :init (gcmh-mode 1))

;; Very large file handling
(use-package vlf
  :straight t)
(use-package vlf-setup
  :config (setq vlf-application 'dont-ask))

;; Jump to char
(use-package avy
  :straight t)

;; Easier keyboard definitions
(use-package general
  :straight t)

;; Completion frontend
(use-package ivy
  :straight t
  :diminish
  :commands ivy-mode
  :bind (("C-x b" . ivy-switch-buffer))
  :init (ivy-mode 1))

;; ivy plugin for minibuffer
(use-package counsel
  :straight t
  :diminish
  :commands (counsel-M-x
               counsel-find-file
               counsel-file-jump
               counsel-recentf
               counsel-rg
               counsel-describe-function
               counsel-describe-variable
               counsel-find-library)
  :bind (("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("C-x f" . counsel-file-jump)
           ("C-x C-r" . counsel-recentf)
           ("C-h f" . counsel-describe-function)
           ("C-h v" . counsel-describe-variable)
           ("C-h l" . counsel-find-library))
  :config (counsel-mode 1))

;; ivy plugin for searching
(use-package swiper
  :straight t)


;; remove minor-modes from modeline
(use-package diminish
  :straight t)

;; minibuffer display for hotkeys
(use-package which-key
  :straight t
  :diminish
  :config (which-key-mode 1))

(defun benn/set-gopath-for-go-projects-hook ()
  "Projectile hook to detect a go project and run go-set-project."
  (if (eq major-mode 'go-mode)
      (go-set-project)))

;; project management
(use-package projectile
  :straight t
  :diminish
  :commands (projectile-mode)
  :init
  (add-hook 'projectile-after-switch-project-hook 'benn/set-gopath-for-go-projects-hook)
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; projectile integration for counsel
(use-package counsel-projectile
  :straight t
  :diminish
  :commands (counsel-projectile-mode)
  :init
  (progn
    (projectile-mode t)
    (counsel-projectile-mode 1)))

;; editorconfig integration for picking up whitespace/indentation settings
(use-package editorconfig
  :straight t
  :diminish
  :config
  (progn
    ;; (setq editorconfig-indentation-alist
    ;;       (append editorconfig-indentation-alist '((vcl-mode c-basic-offset))))
    (editorconfig-mode 1)))

;; Vi modes for emacs
;; (use-package evil
;;   :diminish 'undo-tree-mode
;;   :config
;;   (evil-mode 1)
;;   (delete 'term-mode evil-insert-state-modes)
;;   (add-to-list 'evil-emacs-state-modes 'term-mode))

;; undo mode
;; (use-package undo-tree
;;   :commands global-undo-tree-mode
;;   :init (global-undo-tree-mode 1))

;; flash line when performing evil-mode operations
;; (use-package evil-goggles
;;   :diminish
;;   :config
;;   (evil-goggles-mode)

;;   ;; optionally use diff-mode's faces; as a result, deleted text
;;   ;; will be highlighed with `diff-removed` face which is typically
;;   ;; some red color (as defined by the color theme)
;;   ;; other faces such as `diff-added` will be used for other actions
;;   (evil-goggles-use-diff-faces))

;; Visual regexp
(use-package visual-regexp
  :straight t
  :bind (("M-s r" . vr/query-replace)))

(use-package visual-regexp-steroids
  :straight t)

(use-package pcre2el
  :straight t)

;; Diagnostics for lots of langs
(use-package flycheck
  :straight t
  :diminish
  :init (global-flycheck-mode 1))
(use-package flycheck-inline
  :straight t
  :hook (flycheck-mode . flycheck-inline-mode))
(use-package flycheck-rust
  :straight t)

;; rust integration for flycheck
;; (use-package flycheck-rust
;;   :diminish
;;   :after flycheck)

;; text snippet manager
(use-package yasnippet
  :diminish yas-minor-mode
  :straight t
  :init (yas-global-mode 1))
(use-package yasnippet-snippets
  :straight t
  :config (yas-reload-all))

;; code completion
(use-package company
  :straight t
  :diminish
  :commands global-company-mode
  :bind (:map global-map
              ("C-<tab>" . company-complete-common-or-cycle)
         :map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("TAB" . company-complete-common-or-cycle)
              ("<S-Tab>" . company-select-previous)
              ("<backtab>" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :hook (after-init . global-company-mode)
  :config
  (setq company-require-match 'never
        company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0.25
        company-show-numbers t
        company-selection-wrap-around t
        company-global-modes '(not help-mode helpful-mode)))

;; icons for company
(use-package company-box
  :straight t
  :diminish
  :hook (company-mode . company-box-mode))

(defun benn/term-mode-hook ()
  "Disable line numbers in terminals, as that seems to screw with dimension calculation."
  (display-line-numbers-mode 0))

;; terminals in emacs
(use-package multi-term
  :straight t
  :config
  (progn
    (setq multi-term-program "/bin/bash")
    (setq multi-term-dedicated-select-after-open-p t)
    (setq multi-term-dedicated-skip-other-window-p t)
    (add-hook 'term-mode-hook 'benn/term-mode-hook)))

;; colorful terminals in emacs
(use-package eterm-256color
  :straight t
  :config
  (add-hook 'term-mode-hook 'eterm-256color-mode))

;; Magit
(use-package magit
  :straight t)

;; Language Server Protocol
(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))
(use-package eldoc
  :straight t
  :config
  (setq eldoc-echo-area-use-multiline-p 3
        eldoc-prefer-doc-buffer t
        eldoc-echo-area-display-truncation-message nil))


;; (use-package lsp-mode
;;   :hook (
;;          (prog-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp
;;   :init (setq lsp-keymap-prefix "C-c l"))
;; ;; ui elements for LSP
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . 'lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . 'lsp-ui-peek-find-references)))

;; ivy integration for LSP
;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

;; completions via LSP
;; (use-package company-lsp
;;   :commands company-lsp)

;; treemacs integration with LSP
;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list
;;   :config (lsp-treemacs-sync-mode 1))

;; ================================================
;; UI Packages
;; ================================================
;; pretty icons
(use-package all-the-icons
  :straight t)

;; tree-view of project dirs
;; (use-package treemacs
;;   :defer t)

;; evil-mode integration for treemacs
;; (use-package treemacs-evil
;;   :after treemacs evil)

;; projectile integration for treemacs
;; (use-package treemacs-projectile
;;   :after treemacs projectile)

;; treemacs icons in dired mode
;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :config (treemacs-icons-dired-mode))

;; configure display-line-numbers
(use-package display-line-numbers
  :straight t
  :config
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-grow-only t
        display-line-numbers-width-start t
        display-line-numbers 'relative))

;; auto-focus help windows
(use-package help
  :config (setq help-window-select t))

;; doom modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; ================================================
;; Theme
;; ================================================

(use-package doom-themes
  :straight t
  :config
  (progn
    (setq doom-one-brighter-comments t)
    (load-theme 'doom-one t)))

;; ================================================
;; Language Support
;; ================================================

;; Rust
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :config (setq rust-format-on-save t)
  :hook (rust-mode . eglot-ensure)
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :straight t
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

;;; TOML
(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

; Racket
(use-package racket-mode
  :straight t
  :mode ("\\.rkt\\'" . racket-mode))

;; Org-mode
(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode))

(use-package org-bullets
  :straight t
  :diminish
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Markdown

(use-package markdown-mode
  :mode "\\.md\\'"
  :init
  (use-package request-deferred
    :straight t)
  (use-package github-markdown-preview
    :after request-deferred)
  :straight t)

(use-package markdownfmt
  :straight t
  :bind (:map markdown-mode-map
              ("C-c C-f" . markdownfmt-format-buffer)))


;; VCL
(use-package vcl-mode
  :mode "\\.vcl\\'")

;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :straight t)

;; YAML
(use-package yaml-mode
  :mode ("\\.yaml\\'"
	 "\\.yml\\'")
  :straight t)

;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :straight t)
(use-package docker-compose-mode
  :mode ("docker-compose.yml\\'"
         "docker-compose.yaml\\'")
  :straight t)

;; Groovy/Jenkinsfile
(use-package groovy-mode
  :mode ("\\.groovy\\'"
         "Jenkinsfile\\'")
  :straight t)

;; Web
(use-package web-mode
  :mode "\\.html\\'"
  :straight t)

;; JavaScript
;; lsp support requires running `npm install --global javascript-typescript-langserver` first
(use-package js2-mode
  :mode "\\.js\\'"
  :straight t)

;; Golang
;; lsp support requires running `go get golang.org/x/tools/gopls@latest` first
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'before-save-hook #'gofmt-before-save)
  :straight t)

;; ================================================
;; Functions
;; ================================================

(defun benn/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun benn/last-term-buffer (l)
  "Return most recently used term buffer from buffer list L."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (benn/last-term-buffer (cdr l)))))

(defun benn/get-term ()
  "Switch to the term buffer last used, or create a new one if none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (benn/last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

(defun benn/eol-and-newline ()
  "Move to the end of the line and insert a newline."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun benn/escape ()
  "Quit in current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
  (interactive)
  (when (boundp 'lsp-ui-doc--visible-p)
    (if (lsp-ui-doc--visible-p)
        (lsp-ui-doc-hide)))
  (cond ((active-minibuffer-window)
         (if (minibufferp)
             (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
        ((bound-and-true-p iedit-mode)
         (iedit-quit))
        (t
         ;; ignore top level quits for macros
         (unless (or defining-kbd-macro executing-kbd-macro)
           (keyboard-quit)))))

(defun benn/toggle-lsp-ui-doc ()
  "Toggle display of Documentation-at-point."
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (lsp-ui-doc-hide)
    (lsp-ui-doc-show)))

;; ================================================
;; Keybindings
;; ================================================

;; Override `keyboard-quit` with our universal function
(global-set-key [remap keyboard-quit] 'benn/escape)

;; unbind keys I fat-finger accidentally
(general-unbind
  "C-x C-c")

;; Override emacs globals
(general-define-key
 "C-s" 'swiper
 "M-x" 'counsel-M-x
 "C-x C-b" 'ibuffer
 "M-s-u" 'benn/revert-buffer-no-confirm
 "<f12>" 'multi-term-dedicated-toggle
 ;; "C-`" 'lsp-ui-imenu
 "C-x C-S-c" 'save-buffers-kill-terminal)

;; Code actions via lsp
;; (general-define-key
;;  "C-c r"   'lsp-rename
;;  "C-c e"   '(lsp-treemacs-errors-list :which-key "Show error list")
;;  "C-c f"   'lsp-format-region
;;  "C-c C-f" 'lsp-format-buffer
;;  "C-c TAB" 'lsp-execute-code-action
;;  "C-c i"   '(lsp-ui-peek-find-implementation :which-key "find implementations")
;;  "C-c d"   '(xref-find-definitions :which-key "find definitions")
;;  "C-c D"   '(xref-find-references :which-key "find-references")
;;  "C-c h"   '(benn/toggle-lsp-ui-doc :which-key "Show docs"))

;; LSP keymap
;; (general-define-key
;;  :states '(normal visual insert emacs)
;;  :keymaps 'lsp-mode-map)

;; Code actions via eglot
(general-define-key
 "C-c r" '(eglot-rename :which-key "Rename Symbol")
 "C-c f" '(eglot-format :which-key "Format region")
 "C-c C-f" '(eglot-format-buffer :which-key "Format buffer")
 "C-c TAB" '(eglot-code-actions :which-key "Code Actions")
 "C-c o" '(eglot-code-action-organize-imports :which-key "Organize imports")
 "C-c i" '(eglot-find-implementation :which-key "Find implementations")
 "C-c d" '(xref-find-definitions :which-key "Find declaration")
 "C-c D" '(xref-find-references :which-key "Find references")
 "C-c h" '(eldoc :which-key "Show docs"))

;; setup normal mode evil shortcuts
;; (general-define-key
;;  :states '(normal treemacs)
;;  "gT"  '(switch-to-prev-buffer :which-key "previous buffer")
;;  "gt"  '(switch-to-next-buffer :which-key "next buffer")
;;  "gb"  '(counsel-ibuffer :which-key "buffers list")
;;  "g?"  '(xref-find-references :which-key "find-references")
;;  "g."  '(xref-find-definitions :which-key "find definition")
;;  "g/"  '(lsp-ui-peek-find-implementation : which-key "find implementations")
;;  "`" '(treemacs-select-window :which-key "Show treemacs"))

;; insert/emacs-mode shortcuts
(general-define-key
 "C-<return>" 'benn/eol-and-newline)

(general-unbind "M-SPC")
;; setup evil leader shortcuts
(general-define-key
 :prefix "M-SPC"
 "/"   '(counsel-rg :which-key "ripgrep")
 "TAB" '(mode-line-other-buffer :which-key "previous buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "RET" 'benn/get-term
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
 '(custom-safe-themes
   '("2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" default))
 '(lsp-rust-analyzer-cargo-watch-enable t)
 '(lsp-rust-analyzer-server-display-inlay-hints nil)
 '(lsp-rust-clippy-preference "on")
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-max-height 20)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-hover t)
 '(org-babel-load-languages '((emacs-lisp . t) (shell . t)))
 '(projectile-mode t nil (projectile))
 '(ps-always-build-face-reference t)
 '(ps-default-fg 'frame-parameter)
 '(ps-font-size 12.0)
 '(vr/engine 'pcre2el))

(provide 'init)
;;; init.el ends here
