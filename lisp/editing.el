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

;;; Packages
;;;

;;;; Autorevert
;;;;

(leaf autorevert
  :tag "builtin" "editing"
  :diminish
  :hook ((focus-in-hook after-save-hook) . auto-revert-buffers!)
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

(leaf recentf
  :tag "builtin" "editing"
  :diminish
  :hook (first-file-hook . recentf-mode)
  :commands recentf-open-files
  :custom
  (recentf-filename-handlers .
   '(substring-no-properties    ; strip out lingering text properties
     recent-file-truename       ; resolve symlinks of local files
     abbreviate-file-name))
  (recentf-auto-cleanup . 'never)
  (recentf-max-menu-items . 0)
  (recentf-max-saved-items . 200)
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

(leaf saveplace
  :tag "builtin" "editing"
  :diminish
  :hook (first-file-hook . save-place-mode)
  :init
  (setq save-place-file (concat my-cache-dir "saveplace")
        save-place-limit 100)
  :advice
  (:after-while save-place-find-file-hook
   (lambda (&rest _)
     (if buffer-file-name (ignore-errors (recenter)))))

  (:around save-place-to-alist
   (lambda (orig-fn &rest args)
     (unless large-file-p (apply orig-fn args))))

  (:around save-place-alist-to-file
   (lambda (orig-fn)
     (letf! ((#'pp #'prin1)) (funcall orig-fn)))))

;;;; Savehist
;;;;

(leaf savehist
  :tag "builtin" "editing"
  :diminish
  :hook (first-input-hook . savehist-mode)
  :custom
  `((savehist-file . ,(concat my-cache-dir "savehist"))
    (savehist-save-minibuffer-history . t)
    (savehist-autosave-interval . nil) ; save on kill only
    (savehist-additional-variables . '(kill-ring search-ring regexp-search-ring)))
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

(leaf subword
  :tag "builtin" "editing"
  :diminish
  :hook ((prog-mode-hook . subword-mode)
         (minibuffer-setup-hook . subword-mode)))

;;;; Rect
;;;;

(leaf rect
  :tag "builtin" "editing"
  :bind (("C-x r RET" . rect-hydra/body)
         (:prog-mode-map
          ("C-x r RET" . rect-hydra/body)))
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

(leaf hideshow
  :tag "builtin" "editing"
  :diminish hs-minor-mode
  :bind (:hs-minor-mode-map
         :package hideshow
         ("C-`" . hs-toggle-hiding)))

;;;; Ediff
;;;;

(leaf ediff
  :tag "builtin" "editing"
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer-hook . outline-show-all)
        ;; restore window layout when done
        (ediff-quit-hook . winner-undo))
  :custom
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (ediff-split-window-function . 'split-window-horizontally)
  (ediff-merge-split-window-function . 'split-window-horizontally))

;;;; Electric
;;;;

(leaf elec-pair
  :tag "builtin" "editing"
  :hook (first-file-hook . electric-pair-mode)
  :pre-setq (electric-pair-inhibit-predicate . 'electric-pair-conservative-inhibit))

;;;; IMenu
;;;;

(leaf imenu
  :tag "builtin" "editing"
  :bind (("C-." . imenu))
  :config
  (add-hook 'imenu-after-jump-hook #'recenter))

;;;; Avy
;;;;

(leaf avy
  :ensure t
  :tag "external" "editing"
  :hook (after-init-hook . avy-setup-default)
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
  (avy-all-windows . nil)
  (avy-all-windows-alt . t)
  (avy-background . t)
  (avy-style . 'pre)
  (avy-single-candidate-jump . nil))

(leaf avy-zap
  :ensure t
  :tag "external" "editing"
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;;; Aggressive Indent
;;;;

(leaf aggressive-indent
  :ensure t
  :tag "external" "editing"
  :diminish
  :hook ((after-init-hook . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file-hook . (lambda ()
                             (if (> (buffer-size) (* 3000 80))
                                 (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                         'java-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;;;; Adaptive-Wrap
;;;;

(leaf adaptive-wrap
  :ensure t
  :tag "external" "editing"
  :config
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)))

;;;; Smartparens
;;;;

(leaf smartparens
  :ensure t
  :tag "external" "editing"
  :hook (first-buffer-hook . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :custom
  (sp-highlight-pair-overlay . nil)
  (sp-highlight-wrap-overlay . nil)
  (sp-highlight-wrap-tag-overlay . nil)
  (sp-max-prefix-length . 25)
  (sp-max-pair-length . 4)
  (sp-escape-quotes-after-insert . nil)
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

(leaf ws-butler
  :ensure t
  :doc "A less intrusive `delete-trailing-whitespaces' on save."
  :tag "external" "editing"
  :diminish
  :hook (first-buffer-hook . ws-butler-global-mode))

;;;; EditorConfig
;;;;

(leaf editorconfig
  :ensure t
  :tag "external" "editing"
  :diminish
  :hook ((c-mode-hook c++-mode-hook csharp-mode-hook fsharp-mode-hook java-mode-hook
                      emacs-lisp-mode-hook python-mode-hook ruby-mode-hook
                      markdown-mode-hook css-mode-hook sass-mode-hook html-mode-hook mhtml-mode-hook
                      js-mode-hook js2-mode-hook rjsx-mode-hook typescript-mode-hook json-mode-hook)
         . editorconfig-mode)
  :preface
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
  :custom
  (editorconfig-trim-whitespace-mode . 'ws-butler-mode)
  :advice
  (:around editorconfig-call-editorconfig-exec editorconfig:smart-detection))

;;;; Scratch-Palette
;;;;

(leaf scratch-palette
  :ensure t
  :tag "external" "editing"
  :commands scratch-palette-popup scratch-palette-kill)

(provide 'editing)
;;; editing.el ends here
