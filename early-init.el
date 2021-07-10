;;; early-init.el --- Early initialization for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: January 15, 2021
;; Modified: January 15, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)

(advice-add #'package--ensure-init-file :override #'ignore)

(setq site-run-file nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;(unless (or (daemonp) noninteractive)
;  (let ((old-file-name-handler-alist file-name-handler-alist))
;    (setq-default file-name-handler-alist nil)
;    (defun reset-file-handler-alist ()
;      (setq file-name-handler-alist
;            (delete-dups (append file-name-handler-alist
;                                 old-file-name-handler-alist))))
;    (add-hook 'emacs-startup-hook #'reset-file-handdler-alist)))

(setq user-emacs-directory (file-name-directory load-file-name))

(advice-add #'x-apply-session-resources :override #'ignore)

;;; early-init.el ends here
