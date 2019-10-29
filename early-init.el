;;; early-init.el --- early configurations -*- lexical-binding: t; no-byte-compile: t -*-
;;;
;;; Commentary:
;;; Early-startup Emacs config.
;;;
;;; Code:

(defvar init--gc-cons-threshold gc-cons-threshold)
(defvar init--gc-cons-percentage gc-cons-percentage)
(defvar init--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t
      message-log-max 16384
      file-name-handler-alist nil)
;; after init, restore former values
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold init--gc-cons-threshold
                  gc-cons-percentage init--gc-cons-percentage
                  file-name-handler-alist init--file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defvar package--init-file-ensured)
(setq package-enable-at-startup nil
      package--init-file-ensured t)

(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
