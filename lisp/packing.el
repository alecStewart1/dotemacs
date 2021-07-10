;;; packing.el --- Managing package.el and use-package -*- lexical-binding: t; -*-
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
;;  I use `straight' instead of `package', in order to
;;  more easily use different branches if needed.

;;; Code:

(require 'lib)

;;; Some settings
;;;
(setq gnutls-verify-error t)
(setq package-enable-at-startup nil
      package-user-dir (concat my-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
          ("melpa" . ,(concat proto "://melpa.org/packages/"))
          ("org"   . ,(concat proto "://orgmode.org/elpa/")))))

(advice-add #'package--ensure-init-file :override #'ignore)

;;(add-transient-hook! 'package-install (package-refresh-contents))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t))

;;; For leaf.el
;;;

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(eval-and-compile 
  (unless (package-installed-p 'leaf)
    (add-transient-hook! 'package-install (package-refresh-contents))
    (package-install 'leaf))

  (setq leaf-expand-minimally t)

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf gnu-elpa-keyring-update :ensure t)
    (leaf diminish :ensure t)
    (leaf hydra :ensure t)
    (leaf mode-hydra :ensure t)
    (leaf pretty-hydra :ensure t)
    (leaf general :ensure t)
    :config
    (leaf-keywords-init)))

;;; For straight and use-package
;;;

;; (eval-and-compile
;;   (setq straight-base-dir (file-truename my-local-dir)
;;         straight-repository-branch "develop"
;;         straight-build-dir (format "build-%s" emacs-version)
;;         straight-cache-autoloads nil
;;         straight-check-for-modifications nil
;;         straight-enable-package-integration nil
;;         straight-vc-git-default-clone-depth 1
;;         autoload-compute-prefixes nil))

;; (unless (package-installed-p 'straight)
;;   (defvar bootstrap-version)
;;   (let ((bootstrap-file
;;          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;         (bootstrap-version 5))
;;     (unless (file-exists-p bootstrap-file)
;;       (with-current-buffer
;;           (url-retrieve-synchronously
;;            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;            'silent 'inhibit-cookies)
;;         (goto-char (point-max))
;;         (eval-print-last-sexp)))
;;     (load bootstrap-file nil 'nomessage)))

;; (unless (package-installed-p 'use-package)
;;   (straight-use-package 'use-package))

;; (with-eval-after-load 'straight
;;   (setq straight-use-package-by-default t)
;;   (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

;; ;; Should set before loading `use-package'
;; (eval-and-compile
;;   (setq use-package-verbose t
;;         use-package-always-ensure t
;;         use-package-always-defer t
;;         use-package-expand-minimally t
;;         use-package-compute-statistics t
;;         use-package-enable-imenu-support t))

;; (eval-when-compile
;;   (require 'use-package))

;; (straight-use-package 'gnu-elpa-keyring-update)
;; (straight-use-package 'diminish)
;; (straight-use-package 'bind-key)
;; (straight-use-package 'general)

(provide 'packing)
;;; packing.el ends here
