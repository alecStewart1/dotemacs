;;; writing.el --- For editing text -*- lexical-binding: t; -*-
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

;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'lib)
(require 'ui-ux)
(require 'general)

;;; Some variables we'll use later
;;;

(defvar editing:detect-indent-excluded-modes
  '(fundamental-mode pascal-mode so-long-mode))

(defvar-local editing:inhibit-indent-detection nil)

;;; Keys
;;;


(keymap-global-unset "C-o")
(keymap-global-set "C-o" #'smart-open-line)
(keymap-global-set "C-S-o" #'smart-open-line-above)

;;; Packages
;;;

;;;; Autorevert
;;;;

(use-package autorevert
  :ensure nil
  :diminish
  :hook ((focus-in after-save) . auto-revert-buffers!)
  :init
  (general-setq auto-revert-verbose t
                auto-revert-use-notify nil
                auto-revert-stop-on-user-input nil
                revert-without-query (list "."))
  :config
  (defun auto-revert-buffer! ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (auto-revert-handler)))

  (defun auto-revert-buffers! ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (visible-buffers))
      (with-current-buffer buf
        (auto-revert-buffer!)))))

;;;; Recentf
;;;;

(use-package recentf
  :ensure nil
  :diminish
  ;:hook (after-init . recentf-mode)
  :commands recentf-open-files
  :custom
  (recentf-keep '(file-remote-p file-readable-p))
  (recentf-filename-handlers
   '(substring-no-properties    ; strip out lingering text properties
     recent-file-truename       ; resolve symlinks of local files
     abbreviate-file-name))
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 0)
  (recentf-max-saved-items 200)
  :config
  (defun recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))

  (setq recentf-save-file (concat my-cache-dir "recentf"))

  (add-hook! 'write-file-functions
    (defun recentf-touch-buffer ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))

  (add-hook! 'dired-mode-hook
    (defun recentf-add-dired-directory ()
      "Add dired directory to recentf file list."
      (recentf-add-file default-directory)))

  (add-hook 'kill-emacs-hook (lambda ()
                               (recentf-save-list)
                               (recentf-cleanup))))

;;;; Saveplace
;;;;

(use-package saveplace
  :ensure nil
  :diminish
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (concat my-cache-dir "saveplace")
        save-place-limit 200)
  :config
  (advice-add 'save-place-find-file-hook :after-while
   (lambda (&rest _)
     (if buffer-file-name (ignore-errors (recenter)))))

  (advice-add 'save-place-alist :around
   (lambda (orig-fn &rest args)
     (unless large-file-p (apply orig-fn args))))

  (advice-add 'save-place-alist-to-file :around
   (lambda (orig-fn)
     (letf! ((#'pp #'prin1)) (funcall orig-fn)))))

;;;; Savehist
;;;;

(use-package savehist
  :ensure nil
  :diminish
  :hook (pre-command . savehist-mode)
  :custom
  (savehist-file (concat my-cache-dir "savehist"))
  (savehist-save-minibuffer-history t)
  ; save on kill only
  (savehist-autosave-interval nil)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  (add-hook 'kill-emacs-hook
            (defun savehist:unpropertize-kill-ring ()
              "Remove text properties from `kill-ring' for a smaller savehist file."
              (setq kill-ring (cl-loop for item in kill-ring
                                       if (stringp item)
                                       collect (substring-no-properties item)
                                       else if item collect it)))))

;;;; Subword
;;;;

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;;;; Rect
;;;;

(use-package rect
  :ensure nil
  :bind (("C-x r RET" . rect-hydra/body)
         :map prog-mode-map
         ("C-x r RET" . rect-hydra/body))
  :pretty-hydra
  ((:title "Rectangle" :color amaranth :quit-key "q")
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;;;; Hideshow
;;;;

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
         ("C-`" . hs-toggle-hiding)))

;;;; Ediff
;;;;

(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;;;; Electric
;;;;

(use-package elec-pair
  :ensure nil
  :hook ((find-file . electric-pair-mode)
         (dired-initial-position . electric-pair-mode))
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;;;; IMenu
;;;;

(use-package imenu
  :ensure nil
  :bind (("C-." . imenu))
  :config
  (add-hook 'imenu-after-jump-hook #'recenter))

;;;; Avy
;;;;

(use-package avy
  :hook (after-init . avy-setup-default)
  :preface
  (defun avy:goto-word-beg ()
    "Jump to beginning of word in line."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

  (defun avy:goto-char-2 (char1 char2)
    "Jump to a place that contain the two specified characters."
    (interactive (list (read-char "1> " t)
                       (read-char "2> " t)))
    (avy-with avy-goto-char-2
      (avy-goto-char-2 char1 char2 nil (line-beginning-position) (line-end-position))))

  (defun avy:goto-lisp-cond ()
    "Jump to a Lisp conditional."
    (interactive)
    (avy--generic-jump "\\s(\\(if\\|cond\\|when\\|unless\\)\\b" nil 'pre))
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'pre)
  (avy-single-candidate-jump nil)
  :config
  (emacs:leader-def "g c" #'avy-goto-char)
  (emacs:leader-def "g w" #'avy-goto-word-0)
  (emacs:leader-def "g l" #'avy-goto-line))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;;; Better-Jump
;;;;

(use-package better-jumper
  :hook (pre-command . better-jumper-mode)
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :config
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  (emacs:leader-def "j s" #'better-jumper-set-jump)
  (emacs:leader-def "j j" #'better-jumper-jump-forward)
  (emacs:leader-def "j k" #'better-jumper-jump-backward))

;;;; Easy-Kill
;;;;
;;;; Don’t know if I like these

;; (use-package easy-kill
;;   :bind (([remap kill-ring-save] . easy-kill)
;;          ([remap mark-sexp] . easy-mark)))

;; (use-package easy-kill-extras
;;   :after easy-kill
;;   :config
;;   (require 'extra-things)
;;   (global-set-key [remap mark-word] #'easy-mark-word)
;;   (define-key easy-kill-base-map (kbd "o") #'easy-kill-er-expand)
;;   (define-key easy-kill-base-map (kbd "i") #'easy-kill-er-unexpand)
;;   (add-to-list 'easy-kill-alist '(?W  WORD " ") t)
;;   (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?q  quoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?Q  quoted-string-universal "") t)
;;   (add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
;;   (add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
;;   (add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
;;   (add-to-list 'easy-kill-alist '(?^ backward-line-edge "") t)
;;   (add-to-list 'easy-kill-alist '(?$ forward-line-edge "") t)
;;   (add-to-list 'easy-kill-alist '(?b buffer "") t)
;;   (add-to-list 'easy-kill-alist '(?< buffer-before-point "") t)
;;   (add-to-list 'easy-kill-alist '(?> buffer-after-point "") t)
;;   (add-to-list 'easy-kill-alist '(?f string-to-char-forward "") t)
;;   (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward "") t)
;;   (add-to-list 'easy-kill-alist '(?t string-to-char-backward "") t)
;;   (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward "") t))

;;;; DTRT Indent
;;;;

(use-package dtrt-indent
  :hook ((change-major-mode-after-body read-only-mode) . dtrt:detect-indentation)
  :preface
  (defvar dtrt-indent-run-after-smie)
  :init
  ;; Please
  (setq-default dtrt-indent-verbosity 0)
  :config
  (defun dtrt:detect-indentation ()
    (unless (or (not after-init-time)
                editing:inhibit-indent-detection
                ui-ux:large-file-p
                (memq major-mode editing:detect-indent-excluded-modes)
                (member (substring (buffer-name) 0 1) '(" " "*")))
        (shut-up! (dtrt-indent-mode +1))))

  (setq dtrt-indent-run-after-smie t
        dtrt-indent-max-lines 2000)

  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defadvice! dtrt:fix-broken-smie-modes (orig-fn arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config-guess (beg end)
                (funcall symbol-config-guess beg (min end 1000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[Warning] Indent detection %s"
                                  (error-message-string e))
                         (message "")))))
        (funcall orig-fn arg)))))

;;;; Adaptive-Wrap
;;;;

(use-package adaptive-wrap
  :init
  (setq-default adaptive-wrap-extra-indent 0)
  :config
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)))

;;;; Puni
;;;;

(use-package puni
  :defer t
  :hook ((prog-mode lisp-mode emacs-lisp-mode sgml-mode nxml-mode eval-expression-minibuffer-setup)
         . puni-mode))

;;;; WS-Butler
;;;;

(use-package ws-butler
  :diminish
  :hook (find-file . ws-butler-global-mode))

;;;; EditorConfig
;;;;

;;;###autoload
(defvar editorconfig-mode--alist
  '((emacs-lisp-mode . "el")
    (csharp-mode . "cs")
    (fsharp-mode . "fs")
    (java-mode . "java")
    (js-mode . "js")
    (js2-mode . "js")
    (perl-mode . "pl")
    (php-mode . "php")
    (python-mode . "py")
    (ruby-mode . "rb")
    (sh-mode . "sh")
    (markdown-mode . "md"))
  "An alist mapping major modes to their proper file extensions.
Used in our `editorconfig--smart-detection' function.")

;;;###autoload
(defun editorconfig:smart-detection (orig-fn)
  "Retrieve the properties for the current file. If it doesn't have
an extension, try to guess one."
  (let ((buffer-file-name (if (and (not (bound-and-true-p org-src-mode))
                                   (file-name-extension buffer-file-name))
                              buffer-file-name
                            (format "%s%s" (buffer-file-name (buffer-base-buffer))
                                    (if-let (ext (alist-get major-mode editorconfig-mode--alist))
                                        (concat "." ext)
                                      "")))))
    (funcall orig-fn)))

(use-package editorconfig
  :diminish
  :hook ((c-mode c++-mode csharp-mode fsharp-mode java-mode
                      emacs-lisp-mode python-mode ruby-mode
                      markdown-mode css-mode-hook sass-mode html-mode mhtml-mode
                      js-mode js2-mode rjsx-mode typescript-mode json-mode)
         . editorconfig-mode)
  :custom
  (editorconfig-trim-whitespace-mode #'ws-butler-mode)
  :config
  (advice-add 'editorconfig-call-editorconfig-exec :around #'editorconfig:smart-detection))

;;;; Objed
;;;;

(use-package objed
  ;; :bind ("M-SPC" . objed-activate)
  :defer t
  :preface
  (defvar objed:extra-face-remaps nil)
  :config
  (pushnew! objed-keeper-commands 'undo-fu-only-undo 'undo-fu-only-redo)

  (defadvice! objed:add-face-remaps-a (&rest _)
    "Add extra face remaps when objed activates."
    :after 'objed--init
    (when (memq 'objed-hl (assq 'hl-line face-remapping-alist))
      (push (face-remap-add-relative 'solaire-hl-line-face 'objed-hl)
            objed:extra-face-remaps)))

  (defadvice! objed:remove-face-remaps-a (&rest _)
    "Remove extra face remaps when objed de-activates."
    :after 'objed--reset
    (unless (memq 'objed-hl (assq 'hl-line face-remapping-alist))
      (dolist (remap objed:extra-face-remaps)
        (face-remap-remove-relative remap))
      (setq objed:extra-face-remaps nil)))

  ;(objed-mode +1)
  )

;;;; Scratch-Palette
;;;;

(use-package scratch-palette
  :commands scratch-palette-popup scratch-palette-kill)

(provide 'editing)
;;; editing.el ends here
