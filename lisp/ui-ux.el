;;; ui-ux.el --- To make the UI & UX of Emacs more sensible and modern  -*- lexical-binding: t; -*-
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
;;  A lot of customizations for the UI/UX of Emacs.
;;
;;; Code:

(require 'lib)

;;; My Splash Screen
;;;

;;;###autoload
(defvar my-splash-text (with-temp-buffer
                         (insert-file-contents (expand-file-name (concat user-emacs-directory "splash.txt")))
                         (buffer-string))
  "A string that's the text found in splash.txt.
This is used in my splash screen.")

;;;###autoload
;; TODO actually use this
(defun my-splash ()
  "My personal splash screen for Emacs.

This is more or less a copy of the Nano Emacs splash screen:

https://github.com/rougier/nano-emacs/blob/master/nano-splash.el"
  (interactive)

  (let* ((splash-buffer (get-buffer-create "*Splash*")))
    (with-current-buffer splash-buffer
      (setq header-line-format nil)
      (setq mode-line-format nil)))

  (let* ((splash-buffer   (get-buffer-create "*Splash*"))
         (height          (round (- (window-body-height nil) 1)))
         (width           (round (window-body-width)))
         (paddding-center (+ (/ height 2) 1))
         (title (propertize my-splash-text 'face 'bold))
         (line (propertize "<-=================================================->" 'face 'bold))
         (subtitle (propertize "Alec Stewart's Emacs Config" 'face 'fringe)))
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                               if (buffer-file-name buf)
                               collect (buffer-file-name buf))))
        (with-current-buffer splash-buffer
          (erase-buffer)
          (if (one-window-p) (setq mode-line-format nil))
          (setq cursor-type nil)
          (setq line-spacing 0)
          (setq vertical-scroll-bar nil)
          (setq horizontal-scroll-bar nil)
          (setq fill-column width)
          (if (not (display-graphic-p)) (menu-bar-mode 0))

          (insert-char ?\n paddding-center)
          (insert title)
          (center-line)
          (insert "\n\n")
          (insert line)
          (center-line)
          (insert "\n\n")
          (insert subtitle)
          (center-line)
          (goto-char 0)
          (read-only-mode t)
          (display-buffer-same-window splash-buffer nil)))))

;;; Some Transient keys
;;; TODO may add more in the future

(with-eval-after-load 'transient
  (transient-define-prefix zoom-transient ()
    "Zoomin' in and out on text"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-stay
    [["Zoom"
      ("j" "In"  text-scale-increase)
      ("k" "Out" text-scale-decrease)]])
  (global-set-key (kbd "<f2>") #'zoom-transient))

;;; Packages
;;;

;;;; Help
;;;;

(use-package help
  :ensure nil
  :bind
  (("C-?" . help-command)
   (:map mode-specific-map
    ("h" . help-command)))
  :custom
  (help-window-select t))

(use-package help-fns
  :ensure nil
  :bind ("C-h K" . describe-keymap))

(use-package helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (setq apropos-do-all t)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)
  (defun use-helpful (orig-fn &rest args)
    "Force ORIG-FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply orig-fn args)))
  ;; (with-eval-after-load 'apropos
  ;;   ;; patch apropos buttons to call helpful instead of help
  ;;   (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
  ;;     (button-type-put
  ;;      fun-bt 'action
  ;;      (lambda (button)
  ;;        (helpful-callable (button-get button 'apropos-symbol)))))
  ;;   (dolist (var-bt '(apropos-variable apropos-user-option))
  ;;     (button-type-put
  ;;      var-bt 'action
  ;;      (lambda (button)
  ;;        (helpful-variable (button-get button 'apropos-symbol)))))
  :custom
  ;; Minimuze the amount of unused helpful buffers open
  (helpful-max-buffers 2))

;;;; So-Long
;;;; So we manage working with large files easier.

(defvar ui-ux:inhibit-large-file-detection nil
  "If non-nil, inhibit large/long file detection when opening files.")

(defvar ui-ux:large-file-p nil)
(put 'ui-ux:large-file-p 'permanent-local t)

(defvar ui-ux:large-file-size-alist '(("." . 1.0)))

(defvar ui-ux:large-file-excluded-modes
  '(so-long-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode))

(defadvice! ui-ux:prepare-for-large-files (size _ filename &rest _)
  "From Doom Emacs.
Sets `ui-ux:large-file-p' if the file is considered large.
Uses `ui-ux:large-file-size-alist' to determine when a file is too large. When
`ui-ux:large-file-p' is set, other plugins can detect this and reduce their
runtime costs (or disable themselves) to ensure the buffer is as fast as
possible."
  :before #'abort-if-file-too-large
  (and (numberp size)
       (null ui-ux:inhibit-large-file-detection)
       (ignore-errors
         (> size
            (* 1024 1024
               (assoc-default filename ui-ux:large-file-size-alist
                              #'string-match-p))))
       (setq-local ui-ux:large-file-p size)))

(add-hook! 'find-file-hook
  (defun ui-ux:optimize-for-large-files ()
    "Trigger `so-long-minor-mode' if the file is large."
    (when (and ui-ux:large-file-p buffer-file-name)
      (if (or ui-ux:inhibit-large-file-detection
              (memq major-mode ui-ux:large-file-excluded-modes))
          (kill-local-variable 'ui-ux:large-file-p)
        (when (fboundp 'so-long-minor-mode)
          (so-long-minor-mode +1))
        (message "Large file detected! Cutting a few corners to improve performance...")))))

(use-package so-long
  :ensure nil
  :diminish
  :hook ((find-file . global-so-long-mode)
         (dired-initial-position . global-so-long-mode)
         (org-mode . so-long-minor-mode)
         (prog-mode . so-long-minor-mode))
  :config
  (defun so-long:does-buffer-have-long-lines ()
    (let ((so-long-skip-leading-comments (bound-and-true-p comment-use-syntax))
          (so-long-threshold
           (if visual-line-mode
               (* so-long-threshold
                  (if (derived-mode-p 'text-mode)
                      3
                    2))
             so-long-threshold)))
      (so-long-detected-long-line-p)))

  (setq so-long-threshold 400
        so-long-predicate #'so-long:does-buffer-have-long-lines)
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (add-to-list 'so-long-target-modes 'text-mode)
  (appendq! so-long-minor-modes
            '(flymake-mode
              flycheck-mode
              flyspell-mode
              spell-fu-mode
              eldoc-mode
              smartparens-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-fu-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode)))

;;;; Paren
;;;;

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  ;;(blink-matching-paren t)
  (show-paren-style t)
  (show-paren-delay 0.03)
  (show-paren-highlight-openparen t)
  ;(show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery nil))

;;;; Pulse
;;;;

;;;###autoload
(defun pulse:momentary-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

;;;###autoload
(defun pulse:momentary (&rest _)
  "Pulse the region or the current line."
  (if (fboundp 'xref-pulse-momentarily)
      (xref-pulse-momentarily)
    (pulse:momentary-line)))

;;;###autoload
(defun pulse:recenter-and-pulse (&rest _)
  "Recenter and pulse the region or the current line."
  (recenter)
  (my-pulse-momentary))

;;;###autoload
(defun pulse:recenter-and-pulse-line (&rest _)
  "Recenter and pulse the current line."
  (recenter)
  (pulse:momentary-line))

(use-package pulse
  :ensure nil
  :demand t
  :hook ((bookmark-after-jump
          magit-diff-visit-file
          next-error) . pulse:recenter-and-pulse-line)
  :config
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     forward-page
                     backward-page
                     recenter-top-bottom
                     other-window
                     ace-window
                     aw--select-window
                     windmove-left
                     windmove-right
                     windmove-down
                     windmove-up))
    (advice-add command :after #'pulse:momentary-line))

  (dolist (command '(pop-to-mark-command
                     pop-global-mark
                     goto-last-change))
    (advice-add command :after #'pulse:recenter-and-pulse))

  ;; From here: https://christiantietze.de/posts/2020/12/emacs-pulse-highlight-yanked-text/
  (defadvice! pulse:yank (orig-fn &rest args)
    "Pulse on yanking text, using ‘pulse-momentary-highlight-region’."
    :around #'yank
    (let (begin end)
      (setq begin (point))
      (apply orig-fn args)
      (setq end (point))
      (pulse-momentary-highlight-region begin end)))

  (defadvice! pulse:kill-ring-save (orig-fn &rest args)
    :around #'kill-ring-save
    (let (begin end)
      (setq begin (region-beginning))
      (apply orig-fn args)
      (setq end (point))
      (pulse-momentary-highlight-region begin end)))
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region)))))

;;;; Outline
;;;;

;; Some of this stolen from Oliver Taylor's config:
;; https://github.com/olivertaylor/dotfiles/blob/master/emacs/init.el
;; (use-package outline
;;   :ensure nil
;;   :diminish outline-minor-mode
;;   :hook (prog-mode . global-outline-minor-mode)
;;   :init
;;   (define-global-minor-mode global-outline-minor-mode
;;     outline-minor-mode outline-minor-mode)
;;   :config
;;   (with-eval-after-load 'transient
;;     (transient-define-prefix outline-transient ()
;;       "Transient for Outline Minor Mode navigation"
;;       :transient-suffix 'transient--do-stay
;;       :transient-non-suffix 'transient--do-stay
;;       [["Show/Hide"
;;         ("<right>" "Show Subtree" outline-show-subtree)
;;         ("<left>" "Hide Subtree" outline-hide-subtree)
;;         ("o" "Hide to This Sublevel" outline-hide-sublevels)
;;         ("a" "Show All" outline-show-all)]
;;        ["Navigate"
;;         ("<down>" "Next" outline-next-visible-heading)
;;         ("<up>" "Previous" outline-previous-visible-heading)]
;;        ["Edit"
;;         ("M-<left>"  "Promote" outline-promote)
;;         ("M-<right>" "Demote"  outline-demote)
;;         ("M-<up>"    "Move Up" outline-move-subtree-up)
;;         ("M-<down>"  "Move Down" outline-move-subtree-down)]
;;        ["Other"
;;         ("C-/" "Undo" undo-only)
;;         ("M-/" "Redo" undo-redo)
;;         ("c" "Consult" consult-outline :transient nil)]])))

;;;; Better in-buffer search
;;;;

(use-package ctrlf
  :hook (pre-command . ctrlf-mode))

;;;; Line numbers
;;;;

(use-package display-line-numbers
  :ensure nil
  :hook (find-file . global-display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'visual)
  (display-line-numbers-width 2)
  (display-line-numbers-grow-only t))

;;;; Highlight Line
;;;;
;;;; I hate this

(use-package hl-line
  :ensure nil
  :disabled t
  :commands hl-line-mode global-hl-line-mode)

;;;; Modeline

(use-package nano-modeline
  :diminish nano-modeline-mode
  :init
  (nano-modeline-mode)
  :custom
  (nano-modeline-position 'bottom))

(use-package hide-mode-line
  :hook (completion-list-mode . hide-mode-line-mode))

(use-package minions
  :config
  (minions-mode 1))

;;;; Better moving across windows
;;;;

(use-package windmove
  :ensure nil
  :bind
  (("s-h" . windmove-left)
   ("s-l" . windmove-right)
   ("s-j" . windmove-down)
   ("s-n" . windmove-down)
   ("s-k" . windmove-up)
   ("s-p" . windmove-up)))

(use-package ace-window
  :bind (("s-w" . ace-window)
         ([remap other-window] . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-background t))

;;;; Solaire-Mode
;;;;

(use-package solaire-mode
  :demand t
  :hook ((change-major-mode after-revert) . turn-on-solaire-mode)
  :config
  ;(solaire-mode-swap-bg)
  ;(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
  (add-hook 'solaire-mode-hook (lambda () (set-window-fringes (minibuffer-window) 0 0 nil))))

;;;; Highlighting
;;;;

(use-package highlight-numbers
  :demand t
  :hook ((prog-mode . highlight-numbers-mode)
         (emacs-lisp-mode . highlight-numbers-mode) ;; <- doesn’t enable here?
         (conf-mode . highlight-numbers-mode))
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package hl-todo
  :bind ((:map hl-todo-mode-map
          ([C-f3] . hl-todo-occur)
          ("C-x M-t p" . hl-todo-previous)
          ("C-x M-t n" . hl-todo-next)
          ("C-x M-t o" . hl-todo-occur)))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(use-package diff-hl
  :defer t
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom-face
  (diff-hl-change ((t (:inherit 'highlight))))
  (diff-hl-delete ((t (:inherit 'error :inverse-video t))))
  (diff-hl-insert ((t (:inherit 'success :inverse-video t))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :config
  (diff-hl-margin-mode)
  
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode +1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  (setq diff-hl-draw-borders nil)
  (if (eq system-type 'darwin) (set-fringe-mode '(4 . 8)))

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
          '((insert . " ") (delete . " ") (change . " ")
            (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil)))))

(use-package rainbow-mode
  :diminish
  :bind (:map special-mode-map
         ("w" . rainbow-mode))
  :hook ((web-mode css-mode scss-mode help-mode org-mode) . rainbow-mode)
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun rainbow:clear-overlays ()
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add 'rainbow-turn-off :after #'rainbow:clear-overlays)))

;; TODO might replace this
(use-package rainbow-delimiters
  :demand t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3))

;;;; Shackle
;;;;

(use-package shackle
  :demand t
  :hook (after-init . shackle-mode)
  :preface
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)
  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.28)
  (shackle-rules '(((help-mode helpful-mode)          :align 'below :select t)
                   (comint-mode                       :select t :size 0.4 :align 'below :autoclose t)
                   ("*Completions*"                   :size 0.3 :align 'below :autoclose t)
                   (compilation-mode                  :align 'below :select t)
                   ("*compilation*"                   :align 'below :select nil :size 0.25)
                   ("*Error*"                         :align 'bottom :select nil :size 0.25)
                   ("*package update results*"        :size 0.2 :align 'below :autoclose t)
                   ("*Package-Lint*"                  :size 0.4 :align 'below :autoclose t)
                   ("^\\*macro expansion\\**"         :regexp t :size 0.4 :align 'below)
                   ("^\\*[Wo]*Man.*\\*"               :regexp t :select t :align 'below :autoclose t)
                   ("*Calendar*"                      :select t :size 0.3 :align 'below)
                   ("^\\*Ibuffer\\*$"                 :regexp t :ignore t)
                   ("^\\*image-dired"                 :regexp t :size 0.8 :select t)
                   ("^\\*F\\(?:d\\|ind\\)\\*$"        :regexp t :ignore t)
                   ("*Org Links*"                     :select nil :size 0.2)
                   ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
                    :regexp t :align 'bottom :size 0.25)
                   ("^\\*Org \\(?:Select\\|Attach\\)" :regexp t :align 'bottom :size 0.25)
                   ("^\\*Org Agenda"                  :ignore t)
                   (("^CAPTURE-.*\\.org$" "EXPORT*")  :regexp t :popup t :align 'below :select t :size 0.25)
                   (("*shell*" "*eshell*" "*ielm*")   :popup t :size 0.3 :align 'below)
                   ("*Install vterm*"                 :size 0.35 :same t :align 'below)
                   ("vterm"                           :same t)
                   ("*scheme*"                        :align 'right :size 0.45)
                   ("^\\*sly-mrepl"                   :regexp t :align 'right 0.45)
                   (("^\\*sly-compilation" "^\\*sly-traces" "^\\*sly-description")
                    :regexp t :align 'bottom :size 0.33)
                   (("^\\*sly-\\(?:db\\|inspector\\)") :regexp t :ignore t)
                   ("^\\*geiser messages\\*$"          :regexp t :ignore t)
                   ("^\\*Geiser dbg\\*$"               :regexp t :ignore t)
                   ("^\\*Geiser xref\\*$"              :regexp t)
                   ("^\\*Geiser documentation\\*$"     :regexp t :select t :size 0.35)
                   ("^\\* [A-Za-z0-9_-]+ REPL \\*"     :regexp t)
                   (magit-status-mode                  :align 'bottom :size 0.33 :inhibit-window-quit t)
                   (magit-log-mode                     :same t)
                   (magit-commit-mode                  :ignore t)
                   (magit-diff-mode                    :select nil :align 'right :size 0.5)
                   (git-commit-mode                    :same t)
                   (vc-annotate-mode                   :same t)
                   ("^\\*git-gutter.+\\*$"             :regexp t :size 15 :noselect t)))
  (shackle-select-reused-windows t))

;;;; Themes
;;;;

;;;;; Cleanly load themes
;;;;;
;;;;; Taken from here: https://old.reddit.com/r/emacs/comments/rlli0u/whats_your_favorite_defadvice/

(define-advice load-theme (:before (&rest _args) cleanly-load-themes)
  "Disable all active themes and /then/ load the new theme."
  (mapc #'disable-theme custom-enabled-themes))

;;;;; Theme packages
;;;;;

(use-package doom-themes
  :hook (org-load-hook . doom-themes-org-config))

(use-package nano-theme
  ;:hook (after-init . nano-dark)
  :config
  (load-theme 'nano-dark t))

(provide 'ui-ux)
;;; ui-ux.el ends here
