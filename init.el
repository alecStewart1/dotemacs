;;; init.el --- My init.el -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Copyright (C) 2021 Alec Stewart
;;
;; Created: January 15, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;; Speeding things up
;;;

;;;; Dreaded garbage collection
;;;;

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;;;; Apparently this helps?
;;;;

(setq auto-mode-case-fold nil)

;;;; For PGTK only
;;;;

(when (eq window-system 'pgtk)
  (setq pgtk-wait-for-event-timeout 0.001))

;;;; Deal with that stinky `file-handler-alist'
;;;;

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; If `file-name-handler-alist' is nil, no 256 colors in TUI
    ;; @see https://emacs-china.org/t/spacemacs-centaur-emacs/3802/839
    (setq file-name-handler-alist
          (unless (display-graphic-p)
            '(("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'"
	       . jka-compr-handler))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;;; Add `lisp' and `site-lisp' into our `load-path' 
;;;

(defun update-load-path (&rest _)
  "Update the `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;;; Require our stuff
;;;

(require 'lib)
(require 'packing)
(require 'defaults)
(require 'ui-ux)
(require 'emacsy)
(require 'completion)
(require 'editing)
(require 'termy)
(require 'version-control)

;;; Load the dumb ‘custom-file’
;;;

(when custom-file
  (load custom-file 'noerror))

;;; Setup our special hooks
;;;

(unless noninteractive
  (add-hook 'after-change-major-mode-hook #'run-local-var-hooks 100)
  (run-hook-on 'first-buffer-hook '(find-file-hook switch-buffer-hook))
  (run-hook-on 'first-file-hook   '(find-file-hook dired-initial-position-hook))
  (run-hook-on 'first-input-hook  '(pre-command-hook)))

(setq load-prefer-newer noninteractive)

;;; init.el ends here
