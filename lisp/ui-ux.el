;;; ui-ux.el --- To make the UI & UX of Emacs more sensible and modern  -*- lexical-binding: t; -*-
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

;;; Packages
;;;

;;;; Help
;;;;

(leaf *help-me
  :doc "I need some help."
  :config
  (leaf help
    :tag "builtin" "ui-ux"
    :bind
    (("C-?" . help-command)
     (:mode-specific-map
      :package help
      ("h" . help-command))))

  (leaf helpful
    ;; a better *help* buffer
    :ensure t
    :tag "external" "ui-ux"
    :commands helpful--read-symbol
    :init
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
    (with-eval-after-load 'apropos
      ;; patch apropos buttons to call helpful instead of help
      (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
        (button-type-put
            fun-bt 'action
            (lambda (button)
              (helpful-callable (button-get button 'apropos-symbol)))))
      (dolist (var-bt '(apropos-variable apropos-user-option))
        (button-type-put
            var-bt 'action
            (lambda (button)
              (helpful-variable (button-get button 'apropos-symbol))))))))

;;;; So-Long
;;;;

(leaf so-long
  :tag "builtin" "ui-ux"
  :diminish
  :hook ((first-file-hook . global-so-long-mode)
         (org-mode-hook . so-long-minor-mode)
         (prog-mode-hook . so-long-minor-mode))
  :preface
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
  :setq
  (so-long-threshold . 400)
  (so-long-predicate . #'so-long:does-buffer-have-long-lines)
  :config
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (add-to-list 'so-long-target-modes 'text-mode)
  (appendq! so-long-minor-modes
            '(flycheck-mode
              flyspell-mode
              spell-fu-mode
              eldoc-mode
              smartparens-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode)))

;;;; Paren
;;;;

(leaf paren
  :tag "builtin" "ui-ux"
  :hook (after-init-hook . show-paren-mode)
  :custom
  (blink-matching-paren . 'show)
  (show-paren-style . 'paren)
  (show-paren-delay . 0.03)
  (show-paren-highlight-openparen . t)
  (show-paren-when-point-inside-paren . nil)
  (show-paren-when-point-in-periphery . t)
  :config
  ;; we will call `blink-matching-open` ourselves...
  (remove-hook 'post-self-insert-hook
               #'blink-paren-post-self-insert-function))

;;;; Pulse
;;;;

(leaf pulse
  :tag "builtin" "ui-ux"
  :leaf-defer nil
  :require t
  :preface
  (defun pulse:pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  :config
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
    (advice-add command :after #'pulse:pulse-line)))

;;;; Highlight Line
;;;;

(leaf hl-line
  :doc "I don't like this. It bugs me, so I disable it."
  :tag "builtin" "ui-ux"
  :disabled t
  :commands hl-line-mode global-hl-line-mode)

;;;; TODO Modeline
;;;; ATM it's disabled

(leaf hide-mode-line
  :ensure t
  :tag "external" "ui-ux"
  :hook (((completion-list-mode-hook completion-in-region-mode-hook) . hide-mode-line-mode)))

(leaf minions
  :ensure t
  :tag "external" "ui-ux"
  :config
  (minions-mode 1))

;;;; Better moving across windows
;;;;

(leaf ace-window
  :ensure t
  :tag "external" "ui-ux"
  :bind (([remap other-window]. ace-window))
  :custom
  (aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope . 'frame)
  (aw-background . t))

;;;; Solaire-Mode
;;;;

;(leaf solaire-mode
;  :ensure t
;  :leaf-defer nil
;  :tag "external" "ui-ux"
;  :defun solair-mode-in-minibuffer
;  :global-minor-mode solaire-global-mode
;  :hook ((change-major-mode-hook after-revert-hook) . turn-on-solaire-mode)
;  :config
;  (solaire-mode-swap-bg)
;  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
;  (add-hook 'focus-in-hook #'solaire-mode-reset)
;  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
;  (add-hook 'solaire-mode-hook (lambda () (set-window-fringes (minibuffer-window) 0 0 nil))))

;;;; Highlighting
;;;;

(leaf *highlighting
  :doc "To make Emacs a little prettier."
  :config
  (leaf highlight-numbers
    :ensure t
    :tag "external" "highlight" "ui-ux"
    :hook ((prog-mode-hook conf-mode-hook) . highlight-numbers-mode)
    :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

  (leaf hl-todo
    :ensure t
    :tag "external" "highlight" "ui-ux"
    :bind ((:hl-todo-mode-map
            :package hl-todo
            ([C-f3] . hl-todo-occur)
            ("C-x M-t p" . hl-todo-previous)
            ("C-x M-t n" . hl-todo-next)
            ("C-x M-t o" . hl-todo-occur)))
    :hook (after-init-hook . global-hl-todo-mode)
    :config
    (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
      (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
    (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
      (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

  (leaf diff-hl
    :ensure t
    :tag "external" "highlight" "ui-ux"
    :defvar diff-hl-margin-symbols-alist desktop-minor-mode-table
    :commands diff-hl-magit-post-refresh
    :custom-face
    (diff-hl-change . '((t (:inherit 'highlight))))
    (diff-hl-delete . '((t (:inherit 'error :inverse-video t))))
    (diff-hl-insert . '((t (:inherit 'success :inverse-video t))))
    :bind (:diff-hl-command-map
           :package diff-hl
           ("SPC" . diff-hl-mark-hunk))
    :hook ((after-init-hook . global-diff-hl-mode)
           (dired-mode-hook . diff-hl-dired-mode))
    :config
    ;; Highlight on-the-fly
    (diff-hl-flydiff-mode 1)

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
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

  (leaf volatile-highlights
    :ensure t
    :tag "external" "highlight" "ui-ux"
    :hook (after-init-hook . volatile-highlights-mode))

  (leaf rainbow-mode
    :ensure t
    :tag "external" "highlight" "ui-ux"
    :hook ((web-mode-hook css-mode-hook scss-mode-hook help-mode-hook org-mode-hook) . rainbow-mode)
    :preface
    (defun rainbow:clear-overlays ()
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    :advice
    (:after rainbow-turn-off rainbow:clear-overlays))

  (leaf rainbow-delimiters
    :ensure t
    :leaf-defer nil
    :tag "external" "highlight" "ui-ux"
    :hook (prog-mode-hook . rainbow-delimiters-mode)
    :config
    (setq rainbow-delimiters-max-face-count 3)))

;;;; Shackle
;;;;

(leaf shackle
  :ensure t
  :tag "external" "ui-ux"
  :hook (after-init-hook . shackle-mode)
  :init
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)
  :custom
  (shackle-default-alignment . 'below)
  (shackle-default-size . 0.28)
  (shackle-rules . '(((help-mode helpful-mode)          :align 'below :select t)
                     (comint-mode :select t :size 0.4   :align 'below :autoclose t)
                     ("*Completions*" :size 0.3         :align 'below :autoclose t)
                     (compilation-mode                  :align 'below :select t)
                     ("*compilation*"                   :align 'below :select nil :size 0.25)
                     ("*Error*"                         :align 'bottom :select nil :size 0.25)
                     (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
                     ("*Package-Lint*"                  :size 0.4 :align 'below :autoclose t)
                     ("^\\*macro expansion\\**"         :regexp t :size 0.4 :align 'below)
                     ("\\*[Wo]*Man.*\\*"                :regexp t :select t :align 'below :autoclose t)
                     ("*Calendar*"                      :select t :size 0.3 :align 'below)
                     ("^\\*Ibuffer\\*$"                 :regexp t :ignore t)
                     ("^\\*image-dired"                 :regexp t :size 0.8 :select t)
                     ("*Org Links*"                     :select nil :size 0.2)
                     ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
                      :regexp t :align 'bottom :size 0.25)
                     ("^\\*Org \\(?:Select\\|Attach\\)" :regexp t :align 'bottom :size 0.25)
                     ("^\\*Org Agenda"                  :ignore t)
                     (("^CAPTURE-.*\\.org$" "EXPORT*")  :regexp t :popup t :align 'below :select t :size 0.25)
                     (("*shell*" "*eshell*" "*ielm*")   :popup t :size 0.3 :align 'below)
                     (" *Install vterm* "               :size 0.35 :same t :align 'below)
                     ("vterm"                           :same t)
                     ("*scheme*"                        :align 'right :size 0.45)
                     ("^\\*sly-mrepl"                   :regexp t :align 'right 0.45)
                     (("^\\*sly-compilation" "^\\*sly-traces" "^\\*sly-description") :regexp t :align 'bottom :size 0.33)
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
                     ("^\\*git-gutter.+\\*$"             :regexp t :size 15 :noselect t))))

;;;; Themes
;;;;

;(leaf doom-themes
;  :ensure t
;  :tag "external" "themes" "ui-ux"
;  :hook (org-load-hook . doom-themes-org-config))

(provide 'ui-ux)
;;; ui-ux.el ends here
