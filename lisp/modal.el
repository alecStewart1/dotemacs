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
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ;; ((run-hook-with-args-until-success 'evil:escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

;;;###autoload
(defun evil:escape-i (&rest _)
  "Call `evil:escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'evil:escape)))

(global-set-key [remap keyboard-quit] #'evil:escape)

;;;;; Evil itself
;;;;;

(use-package evil
  :hook (after-init . evil-mode)
  :preface
  (general-setq evil-ex-search-vim-style-regexp t
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
    (add-hook! 'after-save-hook
      (defun vim-like-save-message ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name
                      (file-truename buffer-file-name)
                      (projectile:get-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))

  (defadvice! evil:dont-move-cursor (fn args)
    "HACK ‘=’ moves the cursor to the beginning of selection."
    :around #'evil-indent
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

  ;; Lazy load this stuff
  (delq! 'evil-ex features)
  (add-transient-hook! 'evil-ex (provide 'evil-ex)))


;;;;;; Vim-like Plugins
;;;;;;

(use-package evil-matchit
  :commands evilmi-jump-items evilmi-select-items evilmi-delete-items
  :general
  (:states '(normal visual)
   [remap evil-jump-item] #'evilmi-jump-items))

(use-package evil-numbers
  :after evil
  :general
  (:states '(normal visual)
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
  (add-hook! 'evil:escape-hook
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
  :after (evil evil-ex)
  :config
  (pushnew! evil-traces-argument-type-alist
            '(evil:align . evil-traces-global)
            '(evil:ralign . evil-traces-global))
  (evil-traces-mode))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)

  :config
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;;;;; Text Objects
;;;;;

(use-package evil-textobj-anyblock
  :after evil
  :config
  (setq evil-textobj-anyblock-blocks
        '(("(" . ")")
          ("{" . "}")
          ("\\[" . "\\]")
          ("<" . ">"))))

(use-package exato
  :after evil
  :commands evil-outer-xml-attr evil-inner-xml-attr)

(use-package evil-textobj-tree-sitter
 :after (evil tree-sitter)
 :config
 (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
 (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

;;;;; Keybindings
;;;;;

;;;;;; The Evil Leader Key and Keymap
;;;;;;

(defvar evil:leader-map (make-sparse-keymap "Leader key")
  "The keymap for leader keys for Evil.")

(defvar evil:localleader-map (make-sparse-keymap "Local leader key"))

(general-create-definer evil:leader-def
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-map 'evil:leader-map)

(general-create-definer evil:localleader-def
  :states '(normal visual)
  :prefix "SPC m"
  :prefix-map 'evil:localleader-map)

;;;;;; Leader Key Definitions
;;;;;;


;;;;;;; File Keys
;;;;;;;

(evil:leader-def "f f" #'find-file)
(evil:leader-def "f c" #'vertico:find-file-in-emacsd)
(evil:leader-def "f o" #'vertico:find-file-in-org-dir)
(evil:leader-def "f p" #'vertico:find-file-in-project-root)

;;;;;;; Buffer Keys
;;;;;;;

(evil:leader-def "b b" #'switch-to-buffer)
(evil:leader-def "b w" #'switch-to-buffer-other-window)
(evil:leader-def "b f" #'switch-to-buffer-other-frame)
(evil:leader-def "b i" #'ibuffer)

;;;;;;; Goto Keys
;;;;;;;

(evil:leader-def "g l" #'goto-line)
(evil:leader-def "g o" #'consult-outline)
(evil:leader-def "g m" #'consult-mark)
(evil:leader-def "g k" #'consult-global-mark)
(evil:leader-def "g e" #'consult-error)
(evil:leader-def "g E" #'consult-compile-error)
(evil:leader-def "g i" #'consult-imenu)
(evil:leader-def "g I" #'consult-imenu-multi)

;;;;;;; Search Keys
;;;;;;;

(provide 'modal)
;;; modal.el ends here
