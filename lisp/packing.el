;;; packing.el --- Managing package.el and use-package.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec Stewart

;; Author: Alec Stewart <alec-stewart@protonmail.com>
;; URL: https://github.com/alecStewart1/dotemacs
;; Keywords: emacs .emacs.d dotemacs

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:
;;

;;; Code:

(require 'lib)
(require 'cl-lib)
(require 'comp)

;;; Do stuff with package.el HERE
;;;

;;;; Before we require everything else and packages are compiled, set these
;;;;

(when (and nativecomp-p
           (featurep 'native-compile))
  ;; We're going for distance, we're going for speed
  (setq-default native-comp-compiler-options '("-O2" "-mtune=native")
		native-comp-deferred-compilation t
		comp-deferred-compilation t
		comp-speed 2
		package-native-compile t)

  ;; Don't store eln files in ~/.emacs.d/eln-cache
  (add-to-list 'native-comp-eln-load-path (concat my-cache-dir "eln/"))
  ;; Disable some troublesome packages
  ;; (eval-after-load 'comp
  ;;   (mapc (apply-partially #'add-to-list 'naitve-comp-deferred-compilation-black-list)
  ;;         (let ((local-dir-re (concat "\\`" (regexp-quote my-local-dir))))
  ;;           ;; NOTE: I only use `with-editor', lol
  ;;           (list
  ;;            ;;(concat "\\`" (regexp-quote doom-autoloads-file) "\\'")
  ;;            ;;(concat local-dir-re ".*/evil-collection-vterm\\.el\\'")
  ;;            (concat local-dir-re ".*/with-editor\\.el\\'")
  ;;            ;; https://github.com/nnicandro/emacs-jupyter/issues/297
  ;;            ;;(concat local-dir-re ".*/jupyter-channel\\.el\\'")
  ;;            ))))
  )

;;; Some settings
;;;

(setq gnutls-verify-error t)
(setq package-enable-at-startup nil
      package-user-dir (concat my-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      ;; Also Org ELPA is going to be closed on further releases of Org
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        `(;;("gnu"   . ,(concat proto "://elpa.gnu.org/packages/")) may not actually need this with NonGNU ELPA
          ("melpa" . ,(concat proto "://melpa.org/packages/"))
					("nongnu" . ,(concat proto "://elpa.nongnu.org/nongnu/")))))

(advice-add #'package--ensure-init-file :override #'ignore)

;;; Emacs really like to put stuff in your init.el if you don’t tell it not to
;;;

;;;; HACK: DO NOT WRITE `custome-set-variables' AND FRIENDS TO INIT.EL
;;;;

(defadvice! write-to-sane-paths (fn &rest args)
  "Write 3rd party files to `my-etc-dir' to keep `user-emacs-directory' clean.

Also writes `put' calls for saved safe-local-variables to `custom-file' instead
of `user-init-file' (which `en/disable-command' in novice.el.gz is hardcoded to
do)."
  :around #'en/disable-command
  :around #'locate-user-emacs-file
  (let* ((user-emacs-directory my-etc-dir)
         (custom-file (expand-file-name "custom.el" user-emacs-directory))
         (user-init-file custom-file))
    (apply fn args)))

;;;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;;;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751

(defadvice! dont-write-save-selected-packages (val)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  :override #'package--save-selected-packages
  (when val
    (setq package-selected-packages val)))

;;; Initialize package.el *here*

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(add-transient-hook! 'package-install (package-refresh-contents t))

;;; Useful functions

;; From: https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el#L1305
;;;###autoload
(defun package-menu:upgrade-package ()
  "Mark current package for upgrading (i.e. also mark obsolete version for deletion)."
  (interactive)
  (when-let ((upgrades (package-menu--find-upgrades))
             (description (tabulated-list-get-id))
             (name (package-desc-name description))
             (upgradable (cdr (assq name upgrades))))
    ;; Package is upgradable
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((current-description (tabulated-list-get-id))
               (current-name (package-desc-name current-description)))
          (when (equal current-name name)
            (cond ((equal description current-description)
                   (package-menu-mark-install)
                   (forward-line -1))
                  (t (package-menu-mark-delete)))))
        (forward-line 1)))))

;(define-key 'package-menu-mode-map (kbd "t") #'package-menu:upgrade-package)

;;; For use-package
;;;

(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents t)
    (package-install 'use-package))

  (setq use-package-always-ensure t
        ;use-package-always-defer t
        use-package-expand-minimally t
        use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)
(use-package general)
(use-package ht) ;; <- this is needed in some places
(use-package hydra
  :demand t
  :commands (hydra-move-splitter-up
             hydra-move-splitter-down
             hydra-move-splitter-right
             hydra-move-splitter-left)
  :init
  (setq lv-use-separator t))

(use-package major-mode-hydra
  :functions (major-mode-hydra-define major-mode-hydra-define+))

(use-package pretty-hydra
  :functions (pretty-hydra-define pretty-hydra-define+)
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
       (propertize title 'face face)))))

;;; For leaf.el
;;;
;;; I've found this configuration packge to be a pain to configure.
;;; It expands simply, however, that leads to a lot of problems if you don't
;;; know exactly how things expand and how they are loaded.
;;; I'm lazy. I don't want to do think about that.
;;;
;;; I keep this commented out here for posterity reasons. I might switch back
;;; over in the future.

;; (eval-and-compile 
;;   (unless (package-installed-p 'leaf)
;;     (add-transient-hook! 'package-install (package-refresh-contents))
;;     (package-install 'leaf))

;;   (setq leaf-expand-minimally t)

;;   (leaf leaf-keywords
;;     :ensure t
;;     :config
;;     (leaf gnu-elpa-keyring-update :ensure t)
;;     (leaf diminish :ensure t)
;;     (leaf hydra
;;       :ensure t
;;       :tag "external" "hydra" "ui-ux"
;;       :commands (hydra-move-splitter-up
;;                  hydra-move-splitter-down
;;                  hydra-move-splitter-right
;;                  hydra-move-splitter-left)
;;       :init
;;       (setq lv-use-separator t))
;;     (leaf major-mode-hydra
;;       :ensure t
;;       :tag "external" "hydra" "ui-ux"
;;       :after hydra
;;       :defun (major-mode-hydra-define major-mode-hydra-define+)
;;       :bind (("M-SPC" . major-mode-hydra)))
;;     (leaf pretty-hydra
;;       :ensure t
;;       :tag "external" "hydra" "ui-ux"
;;       :after hydra
;;       :defun (pretty-hydra-define pretty-hydra-define+)
;;       :preface
;;       (cl-defun pretty-hydra-title (title &optional icon-type icon-name
;;                                           &key face height v-adjust)
;;         "Add an icon in the hydra title."
;;         (let ((face (or face `(:foreground ,(face-background 'highlight))))
;;               (height (or height 1.0))
;;               (v-adjust (or v-adjust 0.0)))
;;           (concat
;;            (when (and (icons-displayable-p) icon-type icon-name)
;;              (let ((f (intern (format "all-the-icons-%s" icon-type))))
;;                (when (fboundp f)
;;                  (concat
;;                   (apply f (list icon-name :face face :height height :v-adjust v-adjust))
;;                   " "))))
;;            (propertize title 'face face))))
;;       :config
;;       (with-eval-after-load 'avy
;;         (pretty-hydra-define hydra-avy (:hint nil :color blue :quit-key "q" :title "Avy Things")
;;           ("Characters/Symbols"
;;            (("c" avy-goto-char-in-line "char (1)")
;;             ("C" avy:goto-char-2 "char (2)")
;;             ("x" avy-goto-symbol-1 "symbol"))
;;            "Words"
;;            (("w" avy-goto-word-0 "word")
;;             ("f" avy:goto-word-beg "word beg.")
;;             ("s" avy-goto-subword-1 "subword"))
;;            "Lines"
;;            (("n" avy-goto-line-below "line below")
;;             ("p" avy-goto-line-above "line above")
;;             ("l" avy-copy-line "copy line")
;;             ("m" avy-move-line "move line")
;;             ("e" avy-goto-end-of-line "end of line"))
;;            "Misc"
;;            (("C-c" avy:goto-lisp-cond "Lisp conditional"))))
;;         (global-set-key (kbd "C-c a") #'hydra-avy/body))

;;       (with-eval-after-load 'smartparens
;;         (pretty-hydra-define hydra-sp (:hint nil :quit-key "q" :title "Smartparens")
;;           ("Moving"
;;            (("a" sp-beginning-of-sexp)
;;             ("e" sp-end-of-sexp)
;;             ("f" sp-forward-sexp)
;;             ("b" sp-backward-sexp)
;;             ("n" sp-down-sexp)
;;             ("N" sp-backward-down-sexp)
;;             ("p" sp-up-sexp)
;;             ("P" sp-backward-up-sexp))
;;            "Slurping & Barfing"
;;            (("h" sp-backward-slurp-sexp)
;;             ("H" sp-backward-barf-sexp)
;;             ("l" sp-forward-slurp-sexp)
;;             ("L" sp-forward-barf-sexp))
;;            "Wrapping"
;;            (("R" sp-rewrap-sexp)
;;             ("u" sp-unwrap-sexp)
;;             ("U" sp-backward-unwrap-sexp)
;;             ("(" sp-wrap-round)
;;             ("{" sp-wrap-curly)
;;             ("[" sp-wrap-square))
;;            "Sexp juggling"
;;            (("S" sp-split-sexp)
;;             ("s" sp-splice-sexp)
;;             ("r" sp-raise-sexp)
;;             ("j" sp-join-sexp)
;;             ("t" sp-transpose-sexp)
;;             ("A" sp-absorb-sexp)
;;             ("E" sp-emit-sexp)
;;             ("o" sp-convolute-sexp))
;;            "Destructive editing"
;;            (("c" sp-change-inner :exit t)
;;             ("C" sp-change-enclosing :exit t)
;;             ("k" sp-kill-sexp)
;;             ("K" sp-backward-kill-sexp)
;;             ("w" sp-copy-sexp))))
;;         (define-key 'prog-mode-map (kbd "C-c (") #'hydra-sp/body)))
;;     (leaf-keywords-init)))

(provide 'packing)
;;; packing.el ends here
