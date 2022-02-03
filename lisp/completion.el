;;; completion.el --- Completion in Emacs with Vertico, Consult, Marginalia, and Company with Orderless -*- lexical-binding: t; -*-
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
;;  While I myself am not a minialist, I do appreciate the simplicity yet also featureful-ness of
;;
;;  - Vertico
;;  - Consult
;;  - Marginalia
;;  - and Company
;;
;;  Interesting packages that use the built-in functionalities of Emacs, as opposed to
;;  creating a whole API.
;;
;;; Code:

(require 'lib)
(require 'mode-local)
(require 'thingatpt)
(require 'general)

;;; Packages
;;;

;;;; Orderless
;;;;

(use-package orderless
  :init
  (setq orderless-matching-styles
        '(orderless-literal orderless-initialism orderless-prefixes)
        orderless-component-separator 'orderless-escapable-split-on-space
        completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (orderless partial-completion)))
          (buffer (styles . (orderless partial-completion)))
          (info-menu (styles . (orderless partial-completion)))))
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;;; Minibuffer Completion
;;;;

(use-package minibuffer
  :ensure nil
  :config
  (unless (package-installed-p 'orderless)
    (setq completion-styles '(basic substring flex partial-completion)))
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  :custom
  (tab-always-indent 'complete)
  (completions-format 'vertical)
  (completion-cycle-threshold 3)
  (completion-flex-nospace nil)
  (completion-ignore-case t)
  (completion-pcm-complete-word-inserts-delimiters t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (resize-mini-windows 'grow-only))

;;;;; Vertico
;;;;;

;;;###autoload
(defun vertico:crm-select ()
  "Enter candidate in `consult-completing-read-multiple'"
  (interactive)
  (let ((idx vertico--index))
    (unless (get-text-property 0 'consult--crm-selected (nth vertico--index vertico--candidates))
      (setq idx (1+ idx)))
    (run-at-time 0 nil (lambda ()
                         (interactive)
                         (vertico--goto idx) (vertico--exhibit))))
  (vertico-exit))

;;;###autoload
(defun vertico:crm-exit ()
  "Enter candidate in `consult-completing-read-multiple'"
  (interactive)
  (run-at-time 0 nil #'vertico-exit)
  (vertico-exit))

;;;###autoload
(defun vertico:search-symbol-at-point ()
  "Consult the lines that have the symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defvar vertico:find-file-in--history nil)
;;;###autoload
(defun vertico:find-file-in (&optional dir initial)
    "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote consult-find-args " ")))
    (find-file
     (consult--read
      (split-string (cdr (apply #'simple-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input vertico:find-file-in--history)))))

(defun vertico:find-file-in-emacsd ()
  "Find a file in ‘user-emacs-directory’."
  (interactive)
  (vertico:find-file-in user-emacs-directory))

(defun vertico:find-file-in-org-dir ()
  "Find a file in ‘org-directory’."
  (interactive)
  (vertico:find-file-in org-directory))

(defun vertico:find-file-in-project-root ()
  "Find a file in the ‘projectile-project-root’."
  (interactive)
  (vertico:find-file-in (projectile:get-project-root)))

;;;###autoload
(defun vertico:embark-magit-status (file)
  "From Doom Emacs.
Run ‘magit-status’ on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

(use-package vertico
  :hook (after-init . vertico-mode)
  :functions vertico--exhibit
  :init
  ;; these can be annoying so we disable the help at startup
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil)))
  :custom
  (vertico-resize nil)
  (vertico-count 16)
  (vertico-cycle t)
  :config
  ;; Hooks
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; Keys
  (define-key vertico-map (kbd "<backspace>") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "?") #'minibuffer-completion-help)
  (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exist)
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete))

;;;;; Consult
;;;;;

(use-package consult
  :general
  ;; Remappings
  ([remap switch-to-buffer]              #'consult-buffer)
  ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  ([remap bookmark-jump]                 #'consult-bookmark)
  ([remap yank-pop]                      #'consult-yank-pop)
  ([remap locate]                        #'consult-locate)
  ([remap repeat-complex-command]        #'consult-complex-command)
  ([remap man]                           #'consult-man)
  ([remap imenu]                         #'consult-imenu)
  ([remap goto-line]                     #'consult-goto-line)
  ;; Editing
  ("C-x C-k C-r" #'consult-kmacro)
  ;; Register
  ("C-x r C-r" #'consult-register)
  ("C-x r C-s" #'consult-register-store)
  ;; Histories
  ("C-c h" #'consult-history)
  ;; Isearch
  (:keymaps 'consult-crm-map
   "TAB"                       #'vertico:crm-select
   "RET"                       #'vertico:crm-exit)
  ;; Misc.
  ([remap apropos] #'consult-apropos)
  :preface
  (defvar consult:find-program (cl-find-if #'executable-find (list "fdfind"
                                                                   "fd")))
  :init
  (with-eval-after-load 'projectile
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'projectile:get-project-root))

  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  (general-setq prefix-help-command #'embark-prefix-help-command
                register-preview-delay 0
                register-preview-function #'consult-register-preview
                consult-preview-key '(:debounce 0.2 any)
                consult-narrow-key "<"
                consult-line-numbers-widen t
                consult-async-min-input 2
                consult-async-refresh-delay 0.15
                consult-async-input-throttle 0.2
                consult-async-input-debounce 0.1
                consult-project-root-function #'projectile:get-project-root
                consult-find-args (concat
                                   (format "%s -i -H -E .git --regex %s"
                                           consult:find-program
                                           (if windows-nt-p "--path-separator=/" ""))))
  :config
  (defadvice! consult:recent-file-fix (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  ;; Sources
  ;;

  (defvar consult-org-source
    `(:name     "Org"
      :narrow   ?o
      :hidden   t
      :category buffer
      :state    ,#'consult--buffer-state
      :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'consult-org-source 'append)

  ;; Customizations
  ;;

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (list (kbd "C-SPC") (kbd "M-.")))

  (consult-customize
   consult-buffer consult-buffer-other-window consult-buffer-other-frame
   consult--source-buffer consult--source-project-buffer consult--source-hidden-buffer
   :preview-key (list :debounce 0.25 (kbd "<up>") (kbd "<down>") (kbd "C-p") (kbd "C-n")
                      :debounce 0.3 'any))

  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-SPC") (kbd "M-.")
                      :debounce 1 'any)))

;;;;;; Keybindings
;;;;;;

;;;;;;; Files
;;;;;;;

(emacs:leader-def "f f" #'find-file)
(emacs:leader-def "f r" #'consult-recent-file)
(emacs:leader-def "f c" #'vertico:find-file-in-emacsd)
(emacs:leader-def "f o" #'vertico:find-file-in-org-dir)
(emacs:leader-def "f p" #'vertico:find-file-in-project-root)

;;;;;;; Buffers
;;;;;;;

(emacs:leader-def "b b" #'switch-to-buffer)
(emacs:leader-def "b w" #'switch-to-buffer-other-window)
(emacs:leader-def "b f" #'switch-to-buffer-other-frame)
(emacs:leader-def "b i" #'ibuffer)

;;;;;;; Goto
;;;;;;;

(emacs:leader-def "g m" #'consult-mark)
(emacs:leader-def "g k" #'consult-global-mark)
(emacs:leader-def "g o" #'consult-outline)
(emacs:leader-def "g i" #'consult-imenu)
(emacs:leader-def "g I" #'consult-imenu-multi)
(emacs:leader-def "g e" #'consult-error)
(emacs:leader-def "g E" #'consult-compile-error)

;;;;;;; Searching
;;;;;;;

(emacs:leader-def "s l" #'consult-line)
(emacs:leader-def "s s" #'vertico:search-symbol-at-point)
(emacs:leader-def "s k" #'consult-keep-lines)
(emacs:leader-def "s u" #'consult-focus-lines)
(emacs:leader-def "s m" #'consult-multi-occur)
(emacs:leader-def "s g" #'consult-ripgrep)
(emacs:leader-def "s f" #'consult-find)

;;;;;; Switch Directories in the Minibuffer
;;;;;;

(use-package consult-dir
  :after (consult)
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;;;;; Interface with Flycheck
;;;;;;

(use-package consult-flycheck
  :when (and
         (package-installed-p 'flycheck)
         (bound-and-true-p flycheck-mode))
  :after (consult flycheck))

;;;;; Marginalia
;;;;;

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (advice-add #'marginalia--project-root :override #'projectile:get-project-root)
  (pushnew! marginalia-command-categories
            '(flycheck-error-list-set-filter . builtin)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))

;;;;; Embark
;;;;;

;;; TODO define some Embark maps here
(use-package embark
  :defer t
  :defines vertico-map
  :bind (("C-;" . embark-act)
         ("C-." . embark-dwim)
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("M-e" . embark-export)
         ("M-c" . embark-collect-snapshot)
         :map vertico-map
         ("C-;" . embark-act)
         ("M-e" . embark-export)
         ("M-c" . embark-collect-snapshot))
  :init
  (defun embark:resize-collect-window ()
    (when (memq embark-collect--kind '(:live :completions))
      (fit-window-to-buffer (get-buffer-window)
                            (floor (frame-height) 2) 1)))

  ;(set-face-attribute 'embark-verbose-indicator-title nil :height 1.0)
  :config
  (setq embark-indicator '(embark-highlight-indicator
                           embark-isearch-highlight-indicator
                           embark-verbose-indicator)
        embark-verbose-indicator-display-action
        '(display-buffer-at-bottom (window-height . fit-window-to-buffer)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
  (add-hook 'embark-collect-post-revert-hook #'embark:resize-collect-window)

  (define-key embark-file-map "g" #'vertico:embark-magit-staus))

(use-package embark-consult
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;;; Abbrev

(use-package abbrev
  :ensure nil
  :bind ("<C-tab>" . expand-abbrev))

;;;; Dabbrev
;;;;

(use-package dabbrev
  :ensure nil
  :bind (("C-M-/" . dabbrev-completion)
         ;;("C-M-/" . dabbrev-expand)
         ))

;;;; Hippie Expand
;;;;
;;;; TODO need to adjust this

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-list
     try-expand-line
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-expand-all-abbrevs
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-complete-file-name-partially
     try-complete-file-name)))

;;;; Corfu
;;;;

(use-package corfu
  :hook (after-init . corfu-global-mode)
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("C-n" . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("C-p" . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-quit-no-match 0.5)
  (corfu-quit-at-boundary t)
  (corfu-preselect-first nil)
  (corfu-preview-current t)
  (corfu-scroll-margin 6))

;;;; Cape
;;;;

(use-package cape
  :after corfu
  :preface
  (defvar cape:mega-capf (list
                          (cape-capf-buster
                           (cape-super-capf
                            #'cape-keyword
                            #'cape-symbol
                            #'cape-abbrev
                            #'cape-dabbrev))
                          #'cape-file)
    "A ‘cape-super-capf’ that’s similar to most simpler Company configurations.
The defacto cape to use for coding when none is particularly available.")

  (defvar cape:mega-writing-capf (list
                                  (cape-capf-buster
                                   (cape-super-capf #'cape-dict
                                                    #'cape-abbrev
                                                    #'cape-dabbrev
                                                    #'cape-ispell)))
    "A ‘cape-super-capf’ for modes for writing.
The defacto cape to use for writing when there is none available.")
  :custom
  (cape-dict-file (expand-file-name "~/.local/share/dict/words"))
  (cape-dabbrev-min-length 2)
  :config
  (setq-mode-local prog-mode
                   completion-at-point-functions cape:mega-capf))

(provide 'completion)
;;; completion.el ends here
