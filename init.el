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


;;; Things that speed up startup
;;;

;;;; Dealing with dreaded garbage collection
;;;;

(setq gc-cons-percentage 0.5)

;;;###autolaod
(defconst my-gc-cons-high-threshold most-positive-fixnum)

;;;###autoload
(defconst my-gc-cons-low-threshold 800000)

;;;###autoload
(defconst last-gc-time 0.1)

;;;###autoload
(defvar gc-idle-timer nil)

;;;###autoload
(defmacro gc-time (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;;;###autoload
(defun set-high-threshold ()
  "Set the high GC threshold.
This is to be used with the `pre-command-hook'."
  (setf gc-cons-threshold my-gc-high-cons-threshold))

;;;###autoload
(defun register-idle-gc ()
  "Register a timer to run `idle-garbage-collect'.
Cancel the previous one if present."
  (let ((idle-t (* 20 gcmh-last-gc-time)))
    (when (timerp gc-idle-timer)
      (cancel-timer gc-idle-timer))
    (setf gc-idle-timer
	  (run-with-timer idle-t nil #'idle-collect-garbage))))

;;;###autoload
(defun idle-collect-garbage ()
  "Run garbage collection after `gcmh-idle-delay'."
  (setf last-gc-time (gc-time (garbage-collect)))
  (setf gc-cons-threshold my-gc-cons-low-threshold)
  (setf gc-cons-percentage 0.1))

(progn
  (setf gc-cons-threshold my-gc-cons-high-threshold)
  (add-hook 'pre-command-hook #'set-high-threshold)
  (add-hook 'post-command-hook #'register-idle-gc))

;;;; File name handler
;;;;

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; If `file-name-handler-alist' is nil, no 256 colors in TUI
    ;; @see https://emacs-china.org/t/spacemacs-centaur-emacs/3802/839
    (setq file-name-handler-alist
          (unless (display-graphic-p)
            '(("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))


;;;; Apparently this helps?

(setq auto-mode-case-fold nil)

;;;; For PGTK only

(setq pgtk-wait-for-event-timeout 0.001)

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

;;; Initialize some other things
;;;

(defun my-bootstrap ()
  "Mimicknig Doom Emacs's `doom-initialize' function.
Bootstrap all of my configurations."

  ;; Reset as much state as possible, so `my-bootstrap' can be treated like
  ;; a reset function. e.g. when reloading the config.
  (dolist (var '(exec-path load-path process-environment))
    (set-default var (get var 'initial-value)))

  ;; Require the other configurations
  (require 'packing)
  ;; TODO rest of these are commented out until I figure out
  ;; the issue regarding the blank frozen Emacs frame
  ;(require 'defaults)
  ;(require 'ui-ux)
  ;(require 'emacsy)
  ;(require 'completion)
  ;(require 'term)
  ;(require 'editing)
  ;(require 'writing)
  ;(require 'programming)
  ;(require 'vc)
  ;(require 'app)

  ;; Initialize some hooks
  (dolist (fn '(switch-to-buffer display-buffer))
    (advice-add fn :around #'run-switch-buffer-hooks))

  (add-hook 'after-change-major-mode-hook #'run-local-var-hooks 100)
  (run-hook-on 'first-buffer-hook '(find-file-hook switch-buffer-hook))
  (run-hook-on 'first-file-hook   '(find-file-hook dired-initial-position-hook))
  (run-hook-on 'first-input-hook  '(pre-command-hook))

  ;; Do GC and all that jazz
  (add-hook 'emacs-startup-hook #'my-gc-and-restore))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(major-mode-hydra hydra general diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
