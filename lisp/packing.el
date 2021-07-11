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
    (leaf hydras*
      :doc "Making key combinations a bit saner and nicer looking."
      (leaf hydra
        :ensure t
        :tag "external" "hydra" "ui-ux"
        :commands (hydra-move-splitter-up
                   hydra-move-splitter-down
                   hydra-move-splitter-right
                   hydra-move-splitter-left)
        :init
        (setq lv-use-separator t))
      (leaf mode-hydra
        :ensure t
        :tag "external" "hydra" "ui-ux"
        :after hydra
        :defun (major-mode-hydra-define major-mode-hydra-define+)
        :bind ("M-SPC" . major-mode-hydra))
      (leaf pretty-hydra
        :ensure t
        :tag "external" "hydra" "ui-ux"
        :after hydra
        :defun (pretty-hydra-define pretty-hydra-define+)
        :preface
        (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                            &key face height v-adjust)
          "Add an icon in the hydra title."
          (let ((face (or face `(:foreground ,(face-background 'highlight))))
                (height (or height 1.0))
                (v-adjust (or v-adjust 0.0)))
            (concat
             (when (and (icons-displayable-p) icon-type icon-name)
               (let ((f (intern (format "all-the-icons-%s" icon-type))))
                 (when (fboundp f)
                   (concat
                    (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                    " "))))
             (propertize title 'face face))))
        :config
        (with-eval-after-load 'avy
          (pretty-hydra-define hydra-avy (:hint nil :color blue :quit-key "q" :title "Avy Things")
            ("Characters/Symbols"
             (("c" avy-goto-char-in-line "char (1)"))
             ("C" avy:goto-char-2 "char (2)")
             ("x" avy-goto-symbol-1 "symbol")
             "Words"
             (("w" avy-goto-word-0 "word"))
             ("f" avy:goto-word-beg "word beg.")
             ("s" avy-goto-subword-1 "subword")
             "Lines"
             (("n" avy-goto-line-below "line below"))
             ("p" avy-goto-line-above "line above")
             ("l" avy-copy-line "copy line")
             ("m" avy-move-line "move line")
             ("e" avy-goto-end-of-line "end of line")
             "Misc"
             (("C-c" avy:goto-lisp-cond "Lisp conditional"))))
          (define-key 'text-mode-map (kbd "C-c a") #'hydra-avy/body))

        (with-eval-after-load 'smartparens
          (pretty-hydra-define hydra-sp (:hint nil :quit-key "q" :title "Smartparens")
            ("Moving"
             (("a" sp-beginning-of-sexp)
              ("e" sp-end-of-sexp)
              ("f" sp-forward-sexp)
              ("b" sp-backward-sexp)
              ("n" sp-down-sexp)
              ("N" sp-backward-down-sexp)
              ("p" sp-up-sexp)
              ("P" sp-backward-up-sexp))
             "Slurping & Barfing"
             (("h" sp-backward-slurp-sexp)
              ("H" sp-backward-barf-sexp)
              ("l" sp-forward-slurp-sexp)
              ("L" sp-forward-barf-sexp))
             "Wrapping"
             (("R" sp-rewrap-sexp)
              ("u" sp-unwrap-sexp)
              ("U" sp-backward-unwrap-sexp)
              ("(" sp-wrap-round)
              ("{" sp-wrap-curly)
              ("[" sp-wrap-square))
             "Sexp juggling"
             (("S" sp-split-sexp)
              ("s" sp-splice-sexp)
              ("r" sp-raise-sexp)
              ("j" sp-join-sexp)
              ("t" sp-transpose-sexp)
              ("A" sp-absorb-sexp)
              ("E" sp-emit-sexp)
              ("o" sp-convolute-sexp))
             "Destructive editing"
             (("c" sp-change-inner :exit t)
              ("C" sp-change-enclosing :exit t)
              ("k" sp-kill-sexp)
              ("K" sp-backward-kill-sexp)
              ("w" sp-copy-sexp))))
          (define-key 'prog-mode-map (kbd "C-c (") #'hydra-sp/body))))
    ;(leaf general :ensure t)
    :config
    (setq leaf-defaults '())
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
