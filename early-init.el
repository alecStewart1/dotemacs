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

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq native-comp-deferred-compilation nil)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)

(setq site-run-file nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(unless (or (daemonp) noninteractive)
 (let ((old-file-name-handler-alist file-name-handler-alist))

   (setq-default file-name-handler-alist nil)

   (defun reset-file-handler-alist ()
     (setq file-name-handler-alist
           (delete-dups (append file-name-handler-alist
                                old-file-name-handler-alist))))
   (add-hook 'emacs-startup-hook #'reset-file-handdler-alist 101))


 (setq-default inhibit-redisplay t
               inhibit-message t)
 (add-hook 'window-setup-hook
           (lambda ()
             (setq-default inhibit-redisplay nil
                           inhibit-message nil)
             (redisplay))))

(set-language-environment "UTF-8")

(setq default-input-method nil)

(setq user-emacs-directory (file-name-directory load-file-name))

(advice-add #'x-apply-session-resources :override #'ignore)

;;; early-init.el ends here
