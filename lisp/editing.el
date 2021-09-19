;;; writing.el --- For editing text -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: January 16, 2021
;; Modified: January 16, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'lib)

;;; Things Emacs doesn't enable by default
;;;

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Some variables we'll use later
;;;

(defvar editing:detect-indent-excluded-modes
  '(fundamental-mode pascal-mode so-long-mode))

(defvar-local editing:inhibit-indent-detection nil)

;;; Packages
;;;

;;;; Autorevert
;;;;

(use-package autorevert
  :ensure nil
  :diminish
  :hook ((focus-in after-save) . auto-revert-buffers!)
  :init
  (setq auto-revert-verbose t
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
  :hook (first-file . recentf-mode)
  :commands recentf-open-files
  :custom
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

  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;;;; Saveplace
;;;;

(use-package saveplace
  :ensure nil
  :diminish
  :hook (first-file . save-place-mode)
  :init
  (setq save-place-file (concat my-cache-dir "saveplace")
        save-place-limit 100)
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
  :hook (first-input . savehist-mode)
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
  :hook (first-file . electric-pair-mode)
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
  (avy-single-candidate-jump nil))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;;; DTRT Indent
;;;;

(use-package dtrt-indent
  :hook ((change-major-mode-after-body read-only-mode) . dtrt:detect-indentation)
  :preface
  (defvar dtrt-indent-run-after-smie)
  :config
  (defun dtrt:detect-indentation
      (unless (or (not after-init-time)
                  editing:inhibit-indent-detection
                  ui-ux:large-file-p
                  (memq major-mode editing:detect-indentation-excluded-modes)
                  (member (substring (buffer-name) 0 1) '(" " "*")))
        (dtrt-indent-mode +1)))

  (setq dtrt-indent-run-after-t smie
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

;;;; Aggressive Indent
;;;; This is a bit TOO aggressive.

;; (use-package aggressive-indent
;;   :diminish
;;   :hook ((after-init-hook . global-aggressive-indent-mode)
;;          ;; FIXME: Disable in big files due to the performance issues
;;          ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
;;          (find-file-hook . (lambda ()
;;                              (if (> (buffer-size) (* 3000 80))
;;                                  (aggressive-indent-mode -1)))))
;;   :config
;;   ;; Disable in some modes
;;   (dolist (mode '(asm-mode web-mode html-mode css-mode prolog-inferior-mode))
;;     (push mode aggressive-indent-excluded-modes))

;;   ;; Disable in some commands
;;   (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

;;   ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
;;   (add-to-list
;;    'aggressive-indent-dont-indent-if
;;    '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
;;                          'java-mode)
;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                              (thing-at-point 'line))))))

;;;; Adaptive-Wrap
;;;;

(use-package adaptive-wrap
  :init
  (setq-default adaptive-wrap-extra-indent 0)
  :config
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)))

;;;; Smartparens
;;;;

(use-package smartparens
  :hook (first-buffer . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  (sp-escape-quotes-after-insert nil)
  :config
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil))

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook! 'minibuffer-setup-hook
    (defun smartparens:init-in-minibuffer-maybe? ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression',
`pp-eval-expression' or `evil-ex'."
      (and (memq this-command '(eval-expression pp-eval-expression evil-ex))
           smartparens-global-mode
           (smartparens-mode))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar buffer-smartparens-mode nil)
  ;; (add-hook! 'evil-replace-state-exit-hook
  ;;   (defun smartparens:enable-maybe? ()
  ;;     (when buffer-smartparens-mode
  ;;       (turn-on-smartparens-mode)
  ;;       (kill-local-variable 'buffer-smartparens-mode))))
  ;; (add-hook! 'evil-replace-state-entry-hook
  ;;   (defun smartparens:disable-maybe? ()
  ;;     (when smartparens-mode
  ;;       (setq-local buffer-smartparens-mode t)
  ;;       (turn-off-smartparens-mode))))
  )

;;;; WS-Butler
;;;;

(use-package ws-butler
  :diminish
  :hook (first-buffer . ws-butler-global-mode))

;;;; EditorConfig
;;;;

;;;###autoload
(defvar editorconfig-mode--alist
  '((emacs-lisp-mode . "el")
    (csharp-mode . "cs")
    (fsharp-mode . "fs")
    (java-mode . "java")
    (js-mode . "js")
    (js2-mode "js")
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
  (editorconfig-trim-whitespace-mode 'ws-butler-mode)
  :advice
  (:around editorconfig-call-editorconfig-exec editorconfig:smart-detection))

;;;; Objed
;;;;

(use-package objed
  :bind ("M-SPC" . objed-activate)
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

  (objed-mode +1))

;;;; Scratch-Palette
;;;;

(use-package scratch-palette
  :commands scratch-palette-popup scratch-palette-kill)

(provide 'editing)
;;; editing.el ends here
