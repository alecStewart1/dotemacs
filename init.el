;;; init.el --- My init.el for my Emacs config -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Copyright (C) 2021 Alec Stewart
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

;;; Dealing with garbage collection
;;;

;;;###autoload
(defconst my-gc-cons-threshold (if (display-graphic-p) 20000000 2000000))

;;;###autoload
(defconst my-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000))

;;;###autoload
(defvar my-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbage collectin when idle for 10s.")

(setq gc-cons-threshold my-gc-cons-upper-limit
      gc-cons-percentage 5)

;;; File name handler
;;;

;;;###autoload
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

;;; Some other variables we need here
;;;

;;;; Errors
;;;;

(define-error 'my-config-error "Error with my config")
(define-error 'my-hook-error "An error with a hook of mine" 'my-config-error)

;;;; Environment thingies
;;;;

(dolist (var '(exec-path process-environment))
  (unless (get var 'initial-value)
    (put var 'initial-value (default-value var))))

;;; Restore defaults that I've set
;;;

;;;###autoload
(defun my-gc-and-restore ()
  "Restore some necessary defaults and tell Emacs when to active gc."
  (let ((minibuf-setup (lambda () (setq gc-cons-threshold my-gc-cons-upper-limit)))
        (minibuf-exit (lambda () (setq gc-cons-threshold my-gc-cons-threshold))))
    (setq file-name-handler-alist default-file-name-handler-alist
          gc-cons-threshold my-gc-cons-threshold
          gc-cons-percentage 0.1)
    (if (boundp 'after-focus-change-function)
        (add-function :after after-focus-change-function
                      (lambda ()
                        (unless (frame-focus-state)
                          (garbage-collect))))
      (add-hook 'focus-out-hook #'garbage-collect))

    (add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold my-gc-cons-upper-limit)))
    (add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold my-gc-cons-threshold)))))

;;; Load Path
;;;

;;;###autoload
(defun update-load-path (&rest _)
  "Update the `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;;;###autoload
(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'lib)

;;; Run hooks on a specific function or when switching a buffer.
;;; We need these here for bootstraping

(defun try-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'my-hook-error (list hook e))))
  nil)

(defun run-local-var-hooks ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless inhibit-local-var-hooks
    (setq-local inhibit-local-var-hooks t)
    (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                      #'try-run-hook)))

(defun run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked. Once HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.

TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            (when (and after-init-time
                       (boundp hook)
                       (symbol-value hook))
              (run-hook-wrapped hook-var #'try-run-hook)
              (set hook-var nil))))
      (let ((target (if (eq hook 'find-file-hook) 'after-find-file hook)))
        (if (functionp target)
            (advice-add target :before fn '((depth . -101)))
          (add-hook target fn (if emacs27-p -101)))))))

(defun run-switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or inhibit-switch-buffer-hooks
          (and buffer-or-name
               (eq (current-buffer)
                   (get-buffer buffer-or-name)))
          (and (eq orig-fn #'switch-to-buffer) (car args)))
      (apply orig-fn buffer-or-name args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn buffer-or-name args))
        (with-current-buffer (if (windowp buffer)
                                 (window-buffer buffer)
                               buffer)
          (run-hooks 'switch-buffer-hook))
        buffer))))

;;; Initialize some other things
;;;

(defun my-bootstrap ()
  "Bootstrap all of my configurations."
  ;; Do GC and all that jazz
  (add-hook 'emacs-startup-hook #'my-gc-and-restore)

  (dolist (var '(exec-path process-environment))
    (set-default var (get var 'initial-value)))

  ;; Require the other configurations
  (require 'packing)
  (require 'defaults)
  (require 'ui-ux)
  (require 'emacsy)
  (require 'completion)
  (require 'term)
  (require 'editing)
  (require 'writing)
  (require 'programming)
  (require 'vc)
  (require 'app)
n
  ;; Initialize some hooks
  (dolist (fn '(switch-to-buffer display-buffer))
    (advice-add fn :around #'run-switch-buffer-hooks))

  (add-hook 'after-change-major-mode-hook #'run-local-var-hooks)
  (run-hook-on 'first-buffer-hook '(find-file-hook switch-buffer-hook))
  (run-hook-on 'first-file-hook   '(find-file-hook dired-initial-position-hook))
  (run-hook-on 'first-input-hook  '(pre-command-hook)))

;;; Misc.
;;;

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-prefer-newer noninteractive)

;;; Strap up, boiz!!
;;;

(load (concat (file-name-directory load-file-name) "early-init") nil t)
(my-bootstrap)

;;; init.el ends here
