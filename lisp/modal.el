;;; modal.el --- Experimenting with different modal editing modes -*- lexical-binding: t; -*-
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
;;  I’ve used Evil before, and I’ve briefly touched Boon
;;  Here I want to put a few others.
;;
;;  - Meow
;;  - Evil
;;  - Some kind of Kakoune-like mode
;;
;;  I obviously won’t use them all, but I’d like to explore.
;;  Maybe won’t use anything here, but it’s still nice to have.
;;
;;; Code:

(require 'mode-local)

;;; Packages
;;;

;;;; Meow
;;;;
;; TODO this still needs a bit of work, or rather getting used to.

;; (use-package meow
;;   :demand t
;;   :preface
;;   ;; This is merely from:
;;   ;; https://github.com/meow-edit/meow/blob/master/KEYBINDING_QWERTY.org
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-overwrite-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev))
;;     (meow-leader-define-key
;;      ;; SPC j/k will run the original command in MOTION state.
;;      '("j" . "H-j")
;;      '("k" . "H-k")
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . mode-line-other-buffer)))
;;   :config
;;   (meow-setup)
;;   (meow-global-mode +1))

;;;; Modalka
;;;;
;;;; TODO see if we can kind of replicate Kakoune keys with this

;; (use-package modalka
;;   :preface
;;   (defun kak:insert ()
;;     (modalka-mode 0))

;;   (defun kak:normal ()
;;     (modalka-mode 1))

;;   :config
;;   (defun kak:setup ()
;;     ))

;;;; Evil
;;;;

;;;;; Some quick settings
;;;;;

(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-scroll t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)

;;;;; Leader keymap and General "definer" for leader keys
;;;;;

(defvar evil:leader-map (make-sparse-keymap "Evil Leader Keymap")
  "The keymap for leader keys for Evil.")

(general-create-definer evil:leader-def
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-keymap 'evil:leader-map)

;;;;; Functions for keys
;;;;;

;;;###autoload
(defun evil:shift-right ()
  "vnorempa < <gv"
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun evil:shift-left ()
  "vnorempa > >gv"
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

;;;;;; A better escape
;;;;;;

(defvar evil:escape-hook nil)

(defun evil:escape (&optional interactive)
  (interactive (list interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ((run-hook-with-args-until-success 'evil:escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(defun evil:escape-i (&rest _)
  (when (called-interactively-p 'any)
    (call-interactively #'evil:escape)))

;;;;; Evil itself
;;;;;

(use-package evil
  :defer t
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; TODO
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        ;;evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system
        (cond ((package-installed-p 'undo-fu) 'undo-fu)
              (emacs28-p 'undo-redo)))

  (setq-mode-local magit-mode
                   evil-ex-hl-update-delay 0.25)
  (setq-mode-local so-long-minor-mode
                   evil-ex-hl-update-delay 0.25)
  :config
  (global-set-key [remap keyboard-quit] #'evil:escape)
  (advice-add #'evil-force-normal-state :after #'evil:escape-i)

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; stop copying each visual state move to the clipboard:
  ;; https://github.com/emacs-evil/evil/issues/336
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  (setq-mode-local after-change-major-mode
                   evil-shift-width tab-width)

  (with-eval-after-load 'eldoc
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (unless noninteractive
    (setq save-silently t)
    (add-hook 'after-save-hook
              (defun vim-like-save-message ()
                (message "\%s\" %dL, %dC written"
                         (if buffer-file-name
                             (file-relative-name (file-truename buffer-file-name)
                                                 (projectile:get-project-root))
                           (count-lines (point-min) (point-max))
                           (buffer-size))))))

  (defadvice evil:dont-move-cursor (around evil-indent first (fn args) activate)
    "HACK ‘=’ moves the cursor to the beginning of selection."
    (save-excursion (apply fn args)))

  (defadvice! evil:make-numbered-markers-global (char)
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  ;; REVIEW Fix #2493: dir-locals cannot target fundamental-mode when evil-mode
  ;;        is active. See hlissner/doom-emacs#2493. Revert this if
  ;;        emacs-evil/evil#1268 is resolved upstream.
  (defadvice! evil:fix-local-vars (&rest _)
    :before #'turn-on-evil-mode
    (when (eq major-mode 'fundamental-mode)
      (hack-local-variables)))

  ;; HACK Invoking helpful from evil-ex throws a "No recursive edit is in
  ;;      progress" error because, between evil-ex and helpful,
  ;;      `abort-recursive-edit' gets called one time too many.
  (defadvice! evil:fix-helpful-key-in-evil-ex (key-sequence)
    :before #'helpful-key
    (when (evil-ex-p)
      (run-at-time 0.1 nil #'helpful-key key-sequence)
      (abort-recursive-edit)))

  ;; Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
  ;; spaces. It doesn't in vim, so it shouldn't in evil.
  (defadvice! evil:no-squeeze-on-fill (fn &rest args)
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply fn args)))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties 'evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties 'evil:ralign :ex-arg 'regexp-match)

  ;; Lazy load this stuff
  (delq! 'evil-ex features)
  (add-transient-hook! 'evil-ex (provide 'evil-ex)))

;;;;;; A Bunch of Ex Command Stuff
;;;;;;


;;;;;;; The following allow for using regex
;;;;;;; patterns with evil-ex commands

(defvar evil:flag nil)

(defun evil:ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq evil:flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-lazy-highlight)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq evil:flag 'update))

     ((eq evil:flag 'stop)
      (evil-ex-delete-hl name)))))

(defun evil:ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq evil:flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let* ((pattern (evil-ex-make-substitute-pattern
                         arg
                         (or flags (list))))
               (range (or (evil-copy-range evil-ex-range)
                          (evil-range (or beg (line-beginning-position))
                                      (or end (line-end-position))
                                      'line
                                      :expanded t))))
          (evil-expand-range range)
          (evil-ex-hl-set-region hl-name
                                 (max (evil-range-beginning range) (window-start))
                                 (min (evil-range-end range) (window-end)))
          (evil-ex-hl-change hl-name pattern))
      (end-of-file
       (evil-ex-pattern-update-ex-info nil "incomplete replacement"))
      (user-error
       (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

;;;###autoload
(defun evil-ex-regexp-match (flag &optional arg invert)
  (let ((hl-name 'evil-ex-buffer-match)
        (evil:flag flag))
    (with-selected-window (minibuffer-selected-window)
      (evil:ex-match-init hl-name)
      (cl-destructuring-bind (&optional arg flags)
          (evil-delimited-arguments arg 2)
        (let ((evil-ex-substitute-global
               (if invert
                   (not evil-ex-substitute-global)
                 evil-ex-substitute-global)))
          (evil:ex-buffer-match
           arg hl-name (string-to-list flags)))))))

(evil-ex-define-argument-type regexp-match
                              :runner (lambda (flag &optional arg) (+evil-ex-regexp-match flag arg 'inverted)))
(evil-ex-define-argument-type regexp-global-match
                              :runner +evil-ex-regexp-match)

(defun evil:regexp-match-args (arg)
  (when (evil-ex-p)
    (cl-destructuring-bind (&optional arg flags)
        (evil-delimited-arguments arg 2)
      (list arg (string-to-list flags)))))

;; Other commands can make use of this
(evil-define-interactive-code "<//>"
  :ex-arg regexp-match
  (evil:regexp-match-args evil-ex-argument))

;; The above with bang
(evil-define-interactive-code "<//!>"
  :ex-arg regexp-global-match
  (evil:regexp-match-args evil-ex-argument))

;;;###autoload (autoload 'evil:cd "lisp/modal" nil t)
(evil-define-command evil:cd (&optional path)
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (let ((path (or path "~")))
    (cd path)
    (message "Changed directory to '%s'" (abbreviate-file-name (expand-file-name path)))))

;;;###autoload (autoload 'evil:pwd "lisp/modal" nil t)
(evil-define-command evil:pwd (bang)
  "Display the current working directory. If BANG, copy it to your clipboard."
  (interactive "<!>")
  (if (not bang)
      (pwd)
    (kill-new default-directory)
    (message "Copied to clipboard")))

;;;###autoload (autoload 'evil:align "lisp/modal" nil t)
(evil-define-command evil:align (beg end pattern &optional flags)
  "Ex interface to `align-regexp'.

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
  (interactive "<r><//>")
  (align-regexp
   beg end
   (concat "\\(\\s-*\\)" (evil-transform-vim-style-regexp pattern))
   1 1 (memq ?g flags)))

;;;###autoload (autoload 'evil:ralign "lisp/modal" nil t)
(evil-define-command evil:ralign (beg end pattern &optional flags)
  "Ex interface to `align-regexp' that right-aligns matches.

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
  (interactive "<r><//>")
  (align-regexp
   beg end
   (concat "\\(" (evil-transform-vim-style-regexp pattern) "\\)")
   -1 1 (memq ?g flags)))

;;;###autoload (autoload 'evil:read "lisp/modal" nil t)
(evil-define-command evil:read (count file)
  "Alternative version of `evil-read' that replaces filename modifiers in FILE."
  (interactive "P<fsh>")
  (evil-read count (evil-ex-replace-special-filenames file)))

(with-eval-after-load 'evil-ex
  (evil-ex-define-cmd "cd"  #'evil:cd)
  (evil-ex-define-cmd "pwd" #'evil:pwd)

  (evil-ex-define-cmd "al[ign]"  #'evil:align)
  (evil-ex-define-cmd "ral[ign]" #'evil:ralign)

  (evil-ex-define-cmd "sh[ell]" #'eshell)
  (evil-ex-define-cmd "T[erm]"  #'vterm)

  ;; Org-Mode
  (evil-ex-define-cmd "cap[ture]" #'org-capture)
  (evil-ex-define-cmd "ag[enda]" #'org-agenda)

  (evil-ex-define-cmd "buffers" #'ibuffer)

  ;; Magit
  (evil-ex-define-cmd "git" #'magit-status)
  (evil-ex-define-cmd "gstage" #'magit-stage)
  (evil-ex-define-cmd "gblame" #'magit-blame))

;;;;;; Vim-like Plugins
;;;;;;

(use-package evil-escape
  :commands evil-escape
  :hook (pre-command . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook! 'evil-escape-inhibit-functions
    (defun evil:inhibit-escape-in-minibuffer ()
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p))))))

(use-package evil-matchit
  :commands evilmi-jump-items evilmi-select-items evilmi-delete-items)

(use-package evil-numbers
  :general
  (:states 'normal
   "C-c +" #'evil-numbers/inc-at-pt
   "C-c -" #'evil-numbers/dec-at-pt))

(use-package evil-easymotion
  :after evil
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil)))

  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible))

(use-package evil-exchange
  :commands evil-exchange
  :config
  (add-hook! 'doom-escape-hook
    (defun evil:escape-exchange ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t))))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-dwim] #'evilnc-comment-or-uncomment-lines))

(use-package evil-traces
  :after evil-ex
  :config
  (pushnew! evil-traces-argument-type-alist
            '(evil:align . evil-traces-global)
            '(evil:ralign . evil-traces-global))
  (evil-traces-mode))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;;;;; Text Objects
;;;;;

(use-package evil-textobj-anyblock
  :defer t
  :config
  (setq evil-textobj-anyblock-blocks
        '(("(" . ")")
          ("{" . "}")
          ("\\[" . "\\]")
          ("<" . ">"))))

(use-package exato
  :commands evil-outer-xml-attr evil-inner-xml-attr)

(use-package evil-textobj-tree-sitter
  :after (evil tree-sitter-langs)
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

(provide 'modal)
;;; modal.el ends here
