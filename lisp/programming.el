;;; programming.el --- Configurations for programming -*- lexical-binding: t; -*-
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
;;  Packages that I use for programming, as well as some code
;;  I use to format said code.
;;
;;  TODO
;;  - add in Polymode?
;;  - fix formatter stuff
;;;
;;; Code:

(require 'lib)
(require 'cl-macs)
(require 'cl-lib)
(require 'cl-seq)
(require 'subr-x)
(require 'pcase)
(require 'mode-local)
;(require 'ht)
(require 'dash)
(require 'general)

;;; TODO Personal Code
;;;
;;; Pretend you see none of this for now...

;;;; Formatting Code
;;;; TODO - fix pcase shadowing matches, create format region


;;;###autoload
;; (defvar format:formatter-command-hash (ht ('cs     "clang-format")
;;                                           ('py     "black")
;;                                           ('nim    "nimpretty")
;;                                           ('elixir "mix format")
;;                                           ('perl   "perltidy")
;;                                           ('sh     "shfmt")
;;                                           ('fish   "fish_indent")
;;                                           ('md     "prettierd")
;;                                           ('html   "prettierd")
;;                                           ('js     "prettierd")
;;                                           ('css    "prettierd")
;;                                           ('scss   "prettierd")
;;                                           ('json   "prettierd")
;;                                           ('yaml   "prettierd")))

;; TODO
;; Need to figure out how to account for formatters that can format entire directories
;;;###autoload
;; (defvar format:formatter-args-hash (ht ('cs   " --sort-includes")
;;                                        ('perl " -npro -gnu -nst -nt -dws -b")
;;                                        ('fish " -w")
;;                                        ('md   " -w --prose-wrap --parser markdown")
;;                                        ('html " -w --parser html")
;;                                        ('css  " -w --parser css")
;;                                        ('scss " -w --parser scss")
;;                                        ('json " -w --parser json")
;;                                        ('yaml " -w --parser yaml")))
;; TODO
;;;###autoload
;; (defvar format:formatter-region-args-hash (ht ('py " -c")))

;; (defun format::shfmt-lang-args ()
;;   (let ((lang " -ln "))
;;     (cl-case (and (eql major-mode 'sh-mode)
;;                   (boundp 'sh-shell)
;;                   (symbol-value 'sh-shell))
;;       (bash (concat lang "bash"  " -s"))
;;       (mksh (concat lang "mksh"  " -s"))
;;       (t    (concat lang "posix" " -s")))))

;; (defun format:get-command-for-mode (mode &optional on-region-p)
;;   "Get the commandline command for formatting for the current MODE.
;; ON-REGION-P (not yet implemented) when non-nil is for when we want to only
;; format text in a specific region."
;;   ;; I know, Common Lispers, flet is a stinky
;;   (cl-flet ((get-cmd  (k)
;;                       (ht-get format:formatter-command-hash k))
;;             (get-args (k)
;;                       (if on-region-p
;;                           (ht-get format:formatter-region-args-hash k)
;;                         (ht-get format:formatter-args-hash k))))
;;     (pcase mode
;;       ((or c-mode c++-mode objc-mode)  (concat (get-cmd 'cs) (get-args 'cs)))
;;       (python-mode                     (get-cmd 'py))
;;       (nim-mode                        (get-cmd 'nim))
;;       (elixir-mode                     (get-cmd 'elixir))
;;       (cperl-mode                      (concat (get-cmd 'perl) (get-args 'perl)))
;;       (sh-mode                         (concat (get-cmd 'sh) (format::shfmt-lang-args)))
;;       (fish-mode                       (concat (get-cmd 'fish) (get-args 'fish)))
;;       (markdown-mode                   (concat (get-cmd 'md) (get-args 'md)))
;;       ((or html-mode mhtml-mode)       (concat (get-cmd 'html) (get-args 'html)))
;;       ((or js-mode js2-mode rjsx-mode) (get-cmd 'js))
;;       (css-mode                        (concat (get-cmd 'css) (get-args 'css)))
;;       (scss-mode                       (concat (get-cmd 'scss) (get-args 'scss)))
;;       (json-mode                       (concat (get-cmd 'json) (get-args 'json)))
;;       (yaml-mode                       (concat (get-cmd 'yaml) (get-args 'yaml)))
;;       (_                               #'indent-region))))

;; (defun format:format-current-file ()
;;   "Format the current file in the current buffer."
;;   (interactive)
;;   (let* ((mode    (with-current-buffer (current-buffer) major-mode))
;;          (cmd     (format:get-command-for-mode mode))
;;          (file    (expand-file-name (buffer-file-name))))
;;     (if (stringp cmd)
;;         ;; TODO Is this jank?
;;         (async-shell-command
;;          (concat cmd " " file)
;;          nil t)
;;       ;; If we don't have a formatter for the given langauge, we'll
;;       ;; end up using `indent-region'
;;       (save-excursion
;;         (mark-whole-buffer)
;;         (funcall-interactively cmd (point-min) (point-max))))))

;; TODO
;; The issue with formatting regions is that:
;; 1. Most formatters don't support just passing code from STDIN.
;; 2. In the case of the previous, things begin to get a bit more complex.
;; (defun format:format-region ()
;;   (interactive)
;;   (let* ((mode (with-current-buffer (current-buffer) major-mode))
;;          (cmd  (format:get-command-for-mode mode)))
;;     (if (stringp cmd)
;;         (shell-command-on-region
;;          (region-beginning) (region-end) cmd nil t nil t nil))))

;; (defun format:cheap-indent-sexp ()
;;   "Indent the current s-expression, or next s-expression as `sp-mark-sexp'
;; will use `sp-forward-sexp' if there's a currently marked region.

;; This is \"cheap\" because it just uses `smartparens'.

;; Uses whatever the indent function is for the current mode, as this uses
;; `indent-region'."
;;   (interactive)
;;   (save-excursion
;;     (sp-mark-sexp)
;;     (indent-region (region-beginning) (region-end))))

;;; Packages
;;;

;;;; Utilities
;;;;

;;;;; Compiling things
;;;;;

(use-package compile
  :ensure nil
  :hook (compilation-filter . compile:colorize-compilation-buffer)
  :preface
  (defun compile:colorize-compilation-buffer ()
    "ANSI coloring in the compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :custom
  (compilation-always-kill 1)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

;;;;; Make sure you code doesn’t suck (too much)
;;;;;

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package flymake-collection
  :hook (flymake-mode . flymake-collection-hook-setup))

;;;;; Uh...this thing
;;;;;

(use-package goto-addr
  :hook (prog-mode . goto-address-mode))

;;;;; Get brief documentation in the message area
;;;;;

(use-package eldoc
  :ensure nil
  :diminish
  :init
  (unless emacs28-p
    (setq-default eldoc-documentation-format-function #'eldoc-documentation-format-concat-hr))
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-idle-delay 0.1))

;;;;; Configuration files
;;;;;

(use-package conf-mode
  :mode "\\.cfg\\’")

(use-package yaml-mode
  :mode "\\.yml\\’"
  :mode "\\.yaml\\’"
  :hook (yaml-mode . lsp-deferred))

;;;;; RMSBolt, deeply inspect you code
;;;;;

(use-package rmsbolt
  :after (:any c-mode c++-mode objc-mode emacs-lisp-mode common-lisp-mode python-mode java-mode))

;;;;; Tree-Sitter gives use pretty colors and structural editing
;;;;;

(use-package tree-sitter
  :if (functionp 'module-load)
  :hook (prog-mode . global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :custom
  (tree-sitter-langs
   '((sh-mode . bash)
     (c-mode . c)
     (csharp-mode . c-sharp)
     (c++-mode . cpp)
     (css-mode . css)
     (html-mode . html)
     (mhtml-mode . html)
     (java-mode . java)
     (js-mode . javascript)
     (js2-mode . javascript)
     (ruby-mode . ruby)
     (rjsx-mode . javascript)
     (typescript-mode . typescript)
     (json-mode . json)
     (jsonc-mode . json)
     (python-mode . python)
     (ruby-mode . ruby))))

;;;;; Project.el
;;;;;
;;;;; TODO Finish this up for ignoring folders etc.

(use-package project
  :ensure nil
  :preface
  (defvar project:root-list
    '("Makefile" "GNUMakefile" "CMakeLists.txt"
      "pom.xml"
      "Cask" "Eldev" "Keg" "Eask"
      "package.json" "tsconfig.json" "jsconfig.json"
      "manage.py" "requirements.txt" "setup.py" "tox.ini" "Pipfile" "poetry.lock"
      "Gemfile"
      "info.rkt"))

  (defun project:project-root (&optional dir)
    "Return the current project root of DIR (defaults to ‘default-directory’).
Returns nil if not in a project."
    (if dir
        (project-root dir)
      (project-root (project-current))))

  (defun project:search ()
    "Find files in ‘project-root’ with ripgrep/grep."
    (interactive)
    (let ((proot (project:project-root)))
      (if (executable-find "rg")
          (consult-ripgrep proot)
        (consult-grep proot))))

  (defun project:eshell ()
    "Run an eshell instance in the current project directory."
    (interactive)
    (let* ((default-directory (project:project-root))
           (eshell-buffer-name (project-prefixed-buffer-name "eshell"))
           (eshell-buffer (get-buffer eshell-buffer-name)))
      (if (and eshell-buffer (not current-prefix-arg))
          (pop-to-buffer eshell-buffer '((display-buffer-below-selected . ((window-height . 14)
                                                                    (window-min-height . 8)))))
        (eshell t))))

  :custom
  (project-list-file (concat my-cache-dir "projects.eld"))
  (project-switch-commands
   '((project-dired "Root" "D")
     (project-find-file "File" "f")
     (magit-project-status "Git" "g")
     (project:search "Search" "s")))
  :config
  (define-key ctl-x-map [remap project-search] #'project:search)
  (define-key ctl-x-map [remap project-eshell] #'project:eshell))

;;;; Coding and programming modes
;;;;

;;;;; Assembler
;;;;;

(use-package asm-mode
  :ensure nil
  :mode "\\.inc$")

(use-package nasm-mode
  :mode "\\.nasm$")

(use-package flymake-nasm
  :hook (nasm-mode . flymake-nasm-setup))

;; (use-package mips-mode
;;   :mode "\\.mips$")

(use-package masm-mode
  :if (or windows-nt-p cygwin-p)
  :mode "\\.masm$")

;;;;; C/C++, Objective-C
;;;;;

(use-package cc-mode
  :ensure nil
  :mode ("\\.mm\\'" . objc-mode)
  :hook ((c-mode-local-vars
          c++-mode-local-vars
          objc-mode-local-vars
          cmake-mode-local-vars) . lsp-deferred)
  :hook (c-mode-common . rainbow-delimiters-mode)
  :custom
  ;;(c-default-style "bsd")
  (c-basic-offset tab-width)
  (c-backspace-function #'delete-backward-char))

;; (use-package ccls
;;   :when (package-installed-p 'lsp-mode)
;;   :hook (lsp-lens-mode . ccls-code-lens-mode)
;;   :preface
;;   (defvar ccls-sem-highlight-method 'tree-sitter-hl-mode)
;;   :init
;;   (with-eval-after-load 'lsp-mode
;;     (require 'ccls))
;;   :config
;;   (setq-hook! 'lsp-configure-hook
;;     ccls-sem-highlight-method (if lsp-enable-semantic-highlighting
;;                                   ccls-sem-highlight-method))
;;   (when (or macos-p linux-p)
;;     (setq ccls-initialization-options
;;           `(:index (:trackDependency 1)))))

;;;;; Shell
;;;;;

(use-package sh-script
  :ensure nil
  :defer t
  :mode ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
  :mode ("/bspwmrc\\'" . sh-mode)
  :preface
  (defvar sh-shell-file)
  (defvar sh-builtin-keywords
    '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
      "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
      "sleep" "sudo" "touch")
    "A list of common shell commands to be fontified especially in `sh-mode'.")
  :config
  ;; TODO uhhh...where do I use these two functions?
  (defun sh-script:match-variables-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while (and (setq res (re-search-forward
                               "[^\\]\\(\\$\\)\\({.+?}\\|\\<[a-zA-Z0-9_]+\\|[@*#!]\\)"
                               limit t))
                    (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))

  (defun sh-script:match-command-subst-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while (and (setq res (re-search-forward "[^\\]\\(\\$(.+?)\\|`.+?`\\)"
                                                 limit t))
                    (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))

  (setq sh-indent-after-continuation 'always)

  ;; (add-to-list 'sh-imenu-generic-expression
  ;;              '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
  ;;                   (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))
  
  (add-hook! 'sh-mode-hook
    (defun sh-init-extra-fontification ()
      (font-lock-add-keywords nil
                              `((sh-script:match-variables-in-quotes
                                 (1 'font-lock-constant-face preprend)
                                 (2 'font-lock-variable-name-face prepend))
                                (sh-script:match-command-subst-in-quotes
                                 (1 'sh-quoted-exec prepend))
                                (,(regexp-opt sh-builtin-keywords 'symbols)
                                 (0 'font-lock-type-face append))))))
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode))

(use-package fish-mode
  :config
  (mode-snippet fn fish-mode
    nil
    > "function " @ _ "-d " @ - ?\n
    > @ - ?\n
    >  "end" ?\n))

;;;;; Sieve scripts (not really coding but whatever)
;;;;;
;;;; TODO can we enhance this somehow?

(use-package sieve-mode
  :ensure nil
  :mode "\\.s\\(v\\|iv\\|ieve\\)\\'")

;;;;; Emacs-Lisp
;;;;;

(use-package elisp-mode
  :ensure nil
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  ;;:hook (before-save . format:format-current-file)
  :init
  (defun elisp-mode:indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.
Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned

Also took this from Doom Emacs"
    (let ((normal-indent (current-column))
          (orig-point (point))
          ;; TODO Refactor `target' usage (ew!)
          target)
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
                  (or (not (looking-at-p "\\sw\\|\\s_"))
                      (eq (char-after) ?:)))
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column))
            ((and (save-excursion
                    (goto-char indent-point)
                    (skip-syntax-forward " ")
                    (not (eq (char-after) ?:)))
                  (save-excursion
                    (goto-char orig-point)
                    (and (eq (char-after) ?:)
                         (eq (char-before) ?\()
                         (setq target (current-column)))))
             (save-excursion
               (move-to-column target t)
               target))
            ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                    (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                (get (intern-soft function) 'lisp-indent-hook))))
               (cond ((or (eq method 'defun)
                          (and (null method)
                               (> (length function) 3)
                               (string-match-p "\\`def" function)))
                      (lisp-indent-defform state indent-point))
                     ((integerp method)
                      (lisp-indent-specform method state indent-point normal-indent))
                     (method
                      (funcall method indent-point state))))))))

  :custom
  (tab-width 8)
  (debugger-bury-or-kill 'kill)
  (mode-name "Elisp")
  (outline-regexp "[ \t];;;; [^ \t\n]")
  (lisp-indent-function #'elisp-mode:indent-function)
  :config
  (put 'add-function 'lisp-indent-function 2)
  (put 'advice-add   'lisp-indent-function 2)
  (put 'plist-put    'lisp-indent-function 2)

  (mode-snippet defun emacs-lisp-mode
    nil
    > "(defun " @ - " (" @ _ ")" ?\n
    > "\"" @ _ "\"" ?\n
    > @ _ ")" ?\n)

  (defadvice! elisp-mode:append-val-to-eldoc (orig-fn sym)
    "Display variable value next to documentation in eldoc."
    :around #'elisp-get-var-docstring
    (when-let (ret (funcall orig-fn sym))
      (if (boundp sym)
          (concat ret " "
                  (let* ((truncated " [...]")
                         (print-escape-newlines t)
                         (str (symbol-value sym))
                         (str (prin1-to-string str))
                         (limit (- (frame-width) (length ret) (length truncated) 1)))
                    (format (format "%%0.%ds%%s" (max limit 0))
                            (propertize str 'face 'warning)
                            (if (< (length str) limit) "" truncated)))))))

  (add-hook! 'emacs-lisp-mode-hook
             #'outline-minor-mode
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode
             (lambda ()
               "Disable the checkdoc checker."
               (setq-local flycheck-disabled-checkers
                           '(emacs-lisp-checkdoc))))
  (add-hook 'help-mode-hook 'cursor-sensor-mode))

(use-package ielm
  :ensure nil
  :commands ielm ielm-send-input inferior-emacs-lisp-mode
  :config
  (setq-local scroll-margin 0)
  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  (setq ielm-font-lock-keywords
        (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                   (1 font-lock-comment-face)
                   (2 font-lock-constant-face)))
                (when (require 'highlight-numbers nil t)
                  (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                (cl-loop for (matcher . match-highlights)
                         in (append lisp-el-font-lock-keywords-2
                                    lisp-cl-font-lock-keywords-2)
                         collect
                         `((lambda (limit)
                             (when ,(if (symbolp matcher)
                                        `(,matcher limit)
                                      `(re-search-forward ,matcher limit t))
                               ;; Only highlight matches after the prompt
                               (> (match-beginning 0) (car comint-last-prompt))
                               ;; Make sure we're not in a comment or string
                               (let ((state (syntax-ppss)))
                                 (not (or (nth 3 state)
                                          (nth 4 state))))))
                           ,@match-highlights)))))

(use-package overseer
  :config
  (remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode))

(use-package elisp-demos
  :demand t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package buttercup
  :mode ("/test[/-].+\\.el$" . buttercup-minor-mode)
  :preface
  (defvar buttercup-minor-mode-map (make-sparse-keymap)))

(use-package highlight-quoted
  :defer t)

;;;;; Common Lisp
;;;;;

(defer-feature! lisp-mode)

(use-package sly
  :defer t
  :hook ((lisp-mode-local-vars . sly-editing-mode)
         (sly-mrepl-mode . electric-pair-local-mode))
  :preface
  ;; This causes problems, so we remove it here...
  (remove-hook 'lisp-mode-hook #'sly--lisp-indent-lisp-mode-hook)

  (defvar sly-contribs '(sly-fancy))
  (defvar inferior-lisp-program (concat (executable-find "ros") " -L sbcl-bin -l ~/.sbclrc -Q run"))

  (defvar cape:sly-cape (list
                         (cape-capf-buster
                          (cape-super-capf #'sly-complete-symbol
                                           #'cape-keyword
                                           #'cape-symbol
                                           #'cape-abbrev
                                           #'cape-dabbrev))
                         #'sly-complete-filename-maybe
                         #'cape-file))
  :init
  (setq sly-contribs '(sly-fancy))
  ;;(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook! 'after-init-hook
    (with-eval-after-load 'sly
      (sly-setup)))
  (eval-after-load 'emacs
    (remove-hook 'lisp-mode-hook #'sly-editing-mode))
  (eval-after-load 'lisp
    (remove-hook 'lisp-mode-hook #'sly-editing-mode))
  :config
  (defun sly:cleanup-maybe ()
    "Kill processes and leftover buffers when killing the last sly buffer."
    (let ((buf-list (delq (current-buffer) (buffer-list))))
      (unless (cl-loop for buf in buf-list
                       if (and (buffer-local-value 'sly-mode buf)
                               (get-buffer-window buf))
                       return t)
        (dolist (conn (sly--purge-connections))
          (sly-quit-list-internal conn 'sly-quit-sentinel t))
        (let (kill-buffer-hook kill-buffer-query-functions)
          (mapc #'kill-buffer
                (cl-loop for buf in buf-list
                         if (buffer-local-value 'sly-mode buf)
                         collect buf))))))

  (defun sly:init ()
    "Attempt to auto-start sly when opening a lisp buffer."
    (cl-labels ((temp-buf-p (buf)
                            (equal (substring (buffer-name buf) 0 1) " ")))
      (cond ((or (temp-buf-p (current-buffer))
                 (sly-connected-p)))
            ((executable-find interfior-lisp-program)
             (let ((sly-auto-start 'always))
               (sly-auto-start)
               (add-hook 'kill-buffer-hook #'sly:cleanup-maybe nil t)))
            ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                      inferior-lisp-program)))))

  (add-hook! 'sly-mode-hook #'sly:init)

  ;; ...and add it back in here
  (add-hook 'lisp-mode-hook #'sly--lisp-indent-lisp-mode-hook)

  ;; Cool stuff with Consult
  (defvar consult::sly-mrepl-hist-source
    `(:name "Sly History"
      :narrow ?<
      :face 'font-lock-keyword-face
      :history 'comint-input-ring
      :action (lambda (e)
                (insert e))
      :items ,#'sly-mrepl--read-input-ring))

  (defun conult:sly-mrepl-history ()
    "Select from `sly''s REPL histroy with `consult'."
    (interactive)
    (consult--read consult::sly-mrepl-hist-source
                   :prompt "Item: "
                   :history 'comint-input-ring
                   :require-match t
                   :sort nil
                   :keymap sly-mrepl-mode-map))

  (defvar consult::sly-mrepl-shortcut-source
    `(:name "Shortcut"
      :narrow ?s
      :action
      ,(lambda (string)
               (let ((command (and string
                                   (cdr (assoc string sly-mrepl-shortcut-alist)))))
                 (call-interactively command)))
      :items (mapcar #'car sly-mrepl-shortcut-alist)))

  (defun consult:sly-mrepl-shortcut ()
    "Select a shortcut candidate for `sly''s REPL with `consult'."
    (interactive)
    (consult--read consult::sly-mrepl-shortcut-source
                   :prompt "Action: "
                   :require-match t
                   :sort nil))

  ;; Completions
  (advice-add #'sly-complete-symbol :around
    (lambda (orig-fn)
      (cape-wrap-properties orig-fn :exclusive 'no)))

  (advice-add #'sly-complete-filename-maybe :around
    (lambda (orig-fn)
      (cape-wrap-properties orig-fn :exclusive 'no)))

  (setq-local completion-at-point-functions cape:sly-cape)
  :custom
  (sly-mrepl-history-file-name (concat my-cache-dir "sly-mrepl-history"))
  (sly-net-coding-system 'utf-8-unix)
  (sly-kill-without-query-p t)
  (sly-lisp-implementations
   `((sbcl ("sbcl" "--dynamic-space-size" "2000"))
     (roswell (,(executable-find "ros") " -Q run"))))
  (sly-default-lisp 'roswell)
  (sly-description-autofocus t)
  (sly-inhibit-pipelining nil)
  (sly-load-failed-fasl 'always)
  (sly-ignore-protocol-mismatches t)
  (sly-complete-symbol-function 'sly-flex-completions))

(use-package sly-macrostep
  :after sly)

(use-package sly-quicklisp
  :after sly
  :commands sly-quicklisp)

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-asdf
  :init
  (add-to-list 'sly-contribs 'sly-asdf))

(use-package sly-named-readtables
  :init
  (add-to-list 'sly-contribs 'sly-named-readtables))

;;;;; Schemin’
;;;;;

;; (use-package scheme
;;   :ensure nil
;;   :hook (scheme-mode . rainbow-delimiters-mode)
;;   :preface
;;   (defvar calculate-lisp-indent-last-sexp)
;;   :config
;;   (defadvice! scheme:indent-function (indent-point state)
;;     "A better indenting function for `scheme-mode'."
;;     :override #'scheme-indent-function
;;     (let ((normal-indent (current-column)))
;;       (goto-char (1+ (elt state 1)))
;;       (parse-partial-sexp (point) calculate-lisp-indnet-last-sexp 0 t)
;;       (if (and (elt state 2)
;;                (not (looking-at-p "\\sw\\|\\s_")))
;;           (progn
;;             (unless (> (save-excursion (forward-line 1) (point))
;;                        calculate-lisp-indent-last-sexp)
;;               (goto-char calculate-lisp-indent-last-sexp)
;;               (beginning-of-line)
;;               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
;;             (backward-prefix-chars)
;;             (current-column))
;;         (let* ((function (buffer-substring
;;                           (point)
;;                           (progn
;;                             (forward-sexp 1)
;;                             (point))))
;;                (method (or (get (intern-soft function) 'scheme-indent-function)
;;                            (get (intern-soft function) 'scheme-indent-hook))))
;;           (cond ((or (eq method 'defun)
;;                      (and (null method)
;;                           (> (length function) 3)
;;                           (string-match-p "\\`def" function)))
;;                  (lisp-indent-defform state indent-point))
;;                 ((and (null method)
;;                       (> (length function) 1)
;;                       (string-match-p "\\`:" function))
;;                  (let ((lisp-body-indent 1))
;;                    (lisp-indent-defform state indent-point)))
;;                 ((integerp method)
;;                  (lisp-indent-specform method state indent-point normal-indent))
;;                 (method
;;                  (funcall method state indent-point normal-indent))))))))

;; (use-package geiser
;;   :defer t
;;   :preface
;;   (defun geiser/open-repl ()
;;     "Open the Geiser REPL."
;;     (interactive)
;;     (call-interactively #'switch-to-geiser)
;;     (current-buffer))
;;   :init
;;   ;; To work with Guile + Geiser
;;   ;; We need these first in order to set our `geiser-activate-implementation' variable
;;   (use-package geiser-gauche  :after geiser)
;;   (use-package geiser-chez    :after geiser)
;;   (use-package geiser-chicken :after geiser)
;;   (use-package geiser-guile   :after geiser)
;;   :custom
;;   (geiser-chicken-binary (expand-file-name (executable-find "chicken-csi")))
;;   (geiser-guile-binary (expand-file-name (executable-find "guile3")))
;;   (geiser-active-implementations '(gauche guile chez chicken))
;;   (geiser-default-implementation 'gauche)
;;   (geiser-autodoc-identifier-format "%s => %s")
;;   (geiser-repl-current-project-function #'project:project-root))

;; (use-package macrostep-geiser
;;   :after (:or geiser-mode geiser-repl)
;;   :config
;;   (add-hook 'geiser-mode-hook      #'macrostep-geiser-setup)
;;   (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup))

;; (use-package racket-mode
;;   :mode "\\.rkt\\'"
;;   :hook (racket-mode-local-vars . (racket-xp-mode lsp-deferred))
;;   :preface
;;   (defun racket:open-repl ()
;;     "Open the Racket REPL."
;;     (interactive)
;;     (pop-to-buffer
;;      (or (get-buffer "*Racket REPL*")
;;          (progn (racket-run-and-switch-to-repl)
;;                 (let ((buf (get-buffer "*Racket REPL*")))
;;                   (bury-buffer buf)
;;                   buf)))))
;;   :config
;;   (add-hook! 'racket-mode-hook
;;              #'rainbow-delimiters-mode
;;              #'highlight-quoted-mode)

;;   (add-hook! 'racket-xp-mode-hook
;;     (defun racket-xp-disable-flycheck ()
;;       (cl-pushnew 'racket flyheck-disabled-checkers)))

;;   (define-key 'racket-xp-mode-map [remap racket-doc]              #'racket-xp-documentation)
;;   (define-key 'racket-xp-mode-map [remap racket-visit-definition] #'racket-xp-visit-definition)
;;   (define-key 'racket-xp-mode-map [remap next-error]              #'racket-xp-next-error)
;;   (define-key 'racket-xp-mode-map [remap previous-error]          #'racket-xp-previous-error))

;;;;; Still waiting for the JVM to startup,
;;;;; and for Project Valhalla to be production ready (*quiet sobbing*)

(use-package lsp-java
  :after lsp-mode
  :hook (java-mode-local-vars . java-lsp)
  :init
  (setq lsp-java-workspace-dir (concat my-etc-dir "java-workspace"))
  :config
  ;; Stolen from: https://github.com/dakra/dmacs/blob/master/init.org#java
  (defun java-lsp ()
    (setq electric-indent-inhibit nil)
    (lsp-deferred))
  :custom
  (lsp-jt-root (concat lsp-java-server-install-dir "java-test/server/"))
  (dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))
  ;; Also stolen from: https://github.com/dakra/dmacs/blob/master/init.org#java
  ;; Use Google style formatting by default
  (lsp-java-format-settings-url
   "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (lsp-java-format-settings-profile "GoogleStyle")
  (lsp-java-vmargs
   `("-XX:+UseParallelGC"
     "-XX:GCTimeRatio=4"
     "-Dsun.zip.disableMemoryMapping=true"
     "-noverify"
     "-Xmx1G"
     "-XX:+UseG1GC"
     "-XX:+UseStringDeduplication"
     ,(concat "-javaagent:"
              (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar"))
     ,(concat "-Xbootclasspath/a:"
              (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")))))

;;;###autoload
(defun java:run-test ()
  "Runs test at point.
If in a method, runs the test method, otherwise runs the entire test class."
  (interactive)
  (require 'dap-java)
  (condition-case nil
      (dap-java-run-test-method)
    (user-error (dap-java-run-test-class))))

;;;###autoload
(defun java:debug-test ()
  "Runs test at point in a debugger.
If in a method, runs the test method, otherwise runs the entire test class."
  (interactive)
  (require 'dap-java)
  (condition-case nil
      (call-interactively #'dap-java-debug-test-method)
    (user-error (call-interactively #'dap-java-debug-test-class))))

(use-package groovy-mode
  :mode "\\.g\\(?:radle\\|roovy\\)$")

;;;;; .NET Core
;;;;;

;; (use-package csharp-mode
;;   :hook (csharp-mode . rainbow-delimiters-mode)
;;   :config
;;   (add-hook 'csharp-mode-local-vars-hook #'lsp-deferred))

;; (use-package fsharp-mode
;;   :hook (fsharp-mode . rainbow-delimiters-mode)
;;   :custom
;;   (fsharp-ac-intellisense-enabled nil)
;;   :config
;;   (add-hook 'fsharp-mode-local-vars-hook #'lsp-deferred))

;; TODO make some kinda REPL.
;; TODO doc comment snippet

;; (use-package powershell
;;   :if (or windows-nt-p
;;           cygwin-p
;;           (executable-find "powershell")
;;           (executable-find "pwsh"))
;;   :mode ("\\.ps[dm]?1\\'" . powershell-mode)
;;   :interpreter (("pwsh" . powershell-mode)
;;                 ("powershell" . powershell-mode))
;;   :config
;;   (add-hook 'powershell-mode-hook #'lsp-deferred)
;;   (if (package-installed-p 'dap-mode)
;;       (require 'dap-pwsh)))

;; (use-package sharper
;;   :when (executable-find "dotnet")
;;   :bind ("C-c n" . sharper-main-transient))

;; (use-package bmx-mode
;;   :when (or windows-nt-p cygwin-p)
;;   :config
;;   (bmx-mode-setup-defaults))


;;;;; Go
;;;;;

;; (use-package go-mode
;;   :defer t
;;   :config
;;   (add-hook 'go-mode-local-vars-hook #'lsp-deferred))

;; (use-package gorepl-mode
;;   :commands gorepl-run-load-current-file)

;; (use-package go-gen-test
;;   :after go-mode)

;;;;; ESS
;;;;;
;;;;; I mostly use this for Julia support

;; (use-package ess
;;   :commands stata SAS
;;   :mode ("\\.jl\\'" . ess-julia-mode)
;;   :preface
;;   (defun ess:julia-repl (&optional arg)
;;     "Open an ESS Julia REPL with optional ARG."
;;     (interactive "P")
;;     (run-ess-juila arg)
;;     (current-buffer))
;;   :config
;;   (when (package-installed-p 'lsp-mode)
;;     (add-hook 'ess-julia-mode-local-vars-hook #'lsp-deferred))
;;   :custom
;;   (ess-offset-continued 'straight)
;;   (ess-nuke-trailing-whitespace-p t)
;;   (ess-style 'DEFAULT)
;;   (ess-history-directory (expand-file-name "ess-history/" my-cache-dir)))

;; `ob-julia' needs this variable to be defined
;;;###autoload (defvar inferior-julia-program-name (or (executable-find "julia-basic") "julia"))

(use-package julia-mode
  :interpreter "julia"
  :config
  ;; Borrow matlab.el's fontification of math operators. From
  ;; <https://web.archive.org/web/20170326183805/https://ogbe.net/emacsconfig.html>
  (font-lock-add-keywords
   'julia-mode
   `((,(let ((OR "\\|"))
         (concat "\\("  ; stolen `matlab.el' operators first
                 ;; `:` defines a symbol in Julia and must not be highlighted
                 ;; as an operator. The only operators that start with `:` are
                 ;; `:<` and `::`. This must be defined before `<`.
                 "[:<]:" OR
                 "[<>]=?" OR
                 "\\.[/*^']" OR
                 "===" OR
                 "==" OR
                 "=>" OR
                 "\\<xor\\>" OR
                 "[-+*\\/^&|$]=?" OR  ; this has to come before next (updating operators)
                 "[-^&|*+\\/~]" OR
                 ;; Julia variables and names can have `!`. Thus, `!` must be
                 ;; highlighted as a single operator only in some
                 ;; circumstances. However, full support can only be
                 ;; implemented by a full parser. Thus, here, we will handle
                 ;; only the simple cases.
                 "[[:space:]]!=?=?" OR "^!=?=?" OR
                 ;; The other math operators that starts with `!`.
                 ;; more extra julia operators follow
                 "[%$]" OR
                 ;; bitwise operators
                 ">>>" OR ">>" OR "<<" OR
                 ">>>=" OR ">>" OR "<<" OR
                 "\\)"))
      1 font-lock-type-face))))

;;;;;; Inferior Julia REPL
;;;;;;

;;;###autoload
(defvar julia:repl-start-hook nil)

(use-package julia-repl
  :hook (julia-mode . julia-repl-mode)
  :hook (julia:run-start-hook . julia-repl-use-emacsclient)
  :custom
  (julia-repl-set-terminal-backend 'vterm)
  :config
  (defadvice! julia:run-start-hook (inferior-buf)
    :after #'julia-repl--setup-term
    (with-current-buffer inferior-buf
      (run-hooks 'julia:repl-start-hook)))

  (setq-mode-local julia-mode
    lsp-enable-folding t
    lsp-folding-range-limit 100))

;;;;;; For the LSP support for Julia

(use-package lsp-julia
  :after lsp-mode
  :hook (julia-mode-local-vars . lsp-deferred)
  :preface (setq lsp-julia-default-environment nil)
  :custom
  (lsp-julia-default-environment (expand-file-name "~/.julia/environment/v1.6")))

;;;;; Prolog
;;;;;

(use-package prolog
  :ensure nil
  :when (executable-find "swipl")
  :commands (prolog-mode run-prolog)
  :hook (prolog-mode . lsp-deferred)
  :custom
  (prolog-system 'swi)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (list "swipl"
                                "-g" "use_module(library(lsp_server))."
                                "-g" "lsp_server:main"
                                "-t" "halt"
                                "--" "stdio"))
    :major-modes '(prolog-mode)
    :priority 1
    :multi-root t
    :server-id 'prolog-ls)))

;;;;; Nim
;;;;;

(use-package nim-mode
  :hook (nim-mode . lsp-deferred)
  :init
  (add-hook! 'nim-mode-hook
    (defun nim:init-nimsuggest-mode ()
      "Conditionally load `nimsuggest-mode', instead of clumsily erroring out if
nimsuggest isn't installed."
      (unless (stringp nimsuggest-path)
        (setq nimsuggest-path (executable-find "nimsuggest")))
      (when (and nimsuggest-path (file-executable-p nimsuggest-path))
        (nimsuggest-mode))))

  (when windows-nt-p
    (advice-add #'nimsuggest--get-temp-file-name :filter-return
                (defun nim--suggest-get-temp-file-name (path)
                  (replace-regexp-in-string "[꞉* |<>\"?*]" "" path))))
  ;; :config
  ;; ;; Make use of `dap-mode'
  ;; (require 'dap-gdb-lldb)
  ;; (dap-register-debug-tempalte "Nim::GDB Run Configuration"
  ;;                              (list :type "gdb"
  ;;                                    :request "launch"
  ;;                                    :name "GDB::Run")
  ;;                              :gdbpath "nim-gdb"
  ;;                              :target nil
  ;;                              :cwd nil)
  )

(use-package ob-nim
  :after ob)

;;;;; Erlang
;;;;;

;; (use-package erlang
;;   :mode ("\\.erlang\\'" . erlang-mode)
;;   :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
;;   :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
;;   :config
;;   (when (package-installed-p 'lsp-mode)
;;     (add-hook 'erlang-mode-local-vars-hook #'lsp-deferred)))

;;;;; Elixir
;;;;;

;; (use-package elixir-mode
;;   :defer t
;;   :config
;;   (when (package-installed-p 'lsp-mode)
;;     (add-hook 'elixir-mode-local-vars-hook #'lsp-deferred)
;;     (eval-after-load 'lsp-mode
;;       (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'"))
;;     (require 'dap-elixir))

;;   (eval-after-load 'highlight-numbers
;;     (puthash 'elixir-mode
;;              "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
;;              highlight-numbers-modelist))

;;   ;; (snippet:file-snip mod 'elixir-mode
;;   ;;                    "Name: "
;;   ;;                    ?\n "defmodule " str " do"
;;   ;;                    ?\n > @ _ ?\n
;;   ;;                    "end" ?\n)

;;   ;; (snippet:file-snip def 'elixir-mode
;;   ;;                    "Name: "
;;   ;;                    ?\n "def " @ str " (" @ ") do"
;;   ;;                    ?\n > @ _ ?\n
;;   ;;                    "end" ?\n)

;;   ;; (snippet:file-snip if 'elixir-mode
;;   ;;                    "Condition: "
;;   ;;                    ?\n "if " @ str "do"
;;   ;;                    ?\n > @ _ ?\n
;;   ;;                    "end" ?\n)
;;   )

;; (use-package mix
;;   :after elixir-mode
;;   :hook (elixir-mode . mix-minor-mode))

;; (use-package exunit
;;   :hook (elixir-mode . exunit-mode))

;; (use-package inf-elixir
;;   :general
;;   (:keymaps 'elixir-mode-map
;;    :prefix "C-c i"
;;    "i" #'inf-elixir
;;    "p" #'inf-elixir-project
;;    "l" #'inf-elixir-send-line
;;    "r" #'inf-elixir-send-region
;;    "b" #'inf-elixir-send-buffer))

;; (use-package ob-elixir
;;   :after ob)

;;;;; Perl
;;;;;

;; (use-package cperl-mode
;;   :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
;;   :interpreter (("perl" .     cperl-mode)
;;                 ("perl5" .    cperl-mode)
;;                 ("miniperl" . cperl-mode))
;;   :hook (cperl-mode . (lambda ()
;;                          (set (make-local-variable 'eldoc-documentation-function)
;;                               'cperl:eldoc-doc-function)))
;;   :custom
;;   (cperl-invalid-face nil)
;;   (cperl-font-lock t)
;;   (cperl-electric-keywords t)
;;   (cperl-electric-parens t)
;;   (cperl-info-on-command-no-prompt t)
;;   (cperl-clobber-lisp-bindings t)
;;   (cperl-lazy-help-time t)
;;   :config
;;   (defun cperl:eldoc-doc-function ()
;;     "Return meaningful doc string for `eldoc-mode'."
;;     (car
;;      (let ((cperl-message-on-help-error nil))
;;        (cperl-get-help))))

;;   (add-hook 'cperl-mode-local-vars-hook #'lsp-deferred))

;;;;; Ruby
;;;;;

;; (use-package enh-ruby-mode
;;   :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
;;   :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
;;   :mode "/\\(?:Brew\\|Fast\\)file\\'"
;;   :interpreter "j?ruby\\(?:[0-9.]+\\)"
;;   :config
;;   (add-hook 'enh-ruby-mode-local-vars-hook #'lsp-deferred)
;;   (setq-mode-local enh-ruby-mode sp-max-pair-length 6))

;; (use-package inf-ruby
;;   :after enh-ruby-mode
;;   :config
;;   (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

;; (use-package yard-mode
;;   :after enh-ruby-mode)

;; (use-package rake
;;   :after enh-ruby-mode
;;   :commands (rake rake-rerun rake-regenerate-cache rake-find-task)
;;   :custom
;;   (rake-cache-file (concat my-cache-dir "rake.cache"))
;;   (rake-completion-system 'default))

;; (use-package ruby-test-mode
;;   :after enh-ruby-mode)

;;;;; Python
;;;;;

;; (use-package python
;;   :ensure nil
;;   :mode ("[./]flake8\\'" . conf-mode)
;;   :mode ("/Pipfile\\'" . conf-mode)
;;   :init
;;   (add-hook 'python-mode-local-vars-hook #'lsp-deferred)
;;   :custom
;;   (python-environment-directory my-cache-dir)
;;   (python-indent-guess-indent-offset-verbose nil)
;;   :config
;;   (setq python-indent-guess-indent-offset-verbose nil)
;;   ;; Default to Python 3. Prefer the versioned Python binaries since some
;;   ;; systems stupidly make the unversioned one point at Python 2.
;;   (when (and (executable-find "python3")
;;              (string= python-shell-interpreter "python"))
;;     (setq python-shell-interpreter "python3"))

;;   (define-key python-mode-map (kbd "DEL") nil))

;; (use-package poetry
;;   :after python
;;   :init
;;   (add-hook 'python-mode-hook #'poetry-tracking-mode))

;; (use-package py-isort
;;   :after python
;;   :commands py-isort-buffer py-isort-region)

;; (use-package python-pytest
;;   :after python
;;   :commands python-pytest-dispatch)

;; (use-package live-py-mode
;;   :after python
;;   :commands live-py-mode)

;; (use-package lsp-pyright
;;   :after (:all lsp-mode python))

;;;;; lol webdev (*more quiet sobbing*)
;;;;;

(use-package sgml-mode
  :ensure nil
  :hook (html-mode . (sgml-electric-tag-pair-mode
                      sgml-name-8bit-mode))
  :custom
  (sgml-basic-offset 2))

(use-package mhtml-mode
  :ensure nil
  :hook (html-mode . mhtml-mode))

(add-hook 'css-mode-hook #'lsp-deferred)

(use-package nxml-mode
  :ensure nil
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.xaml\\'"
  :mode "\\.rss\\'"
  :magic "<\\?xml"
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

;;;###autoload
(defun ts:init-lsp-maybe ()
  "Start ‘lsp’ in current buffer."
  (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
    (when (derived-mode-p 'js-mode 'typescript-mode)
      (if (null buffer-file-name)
          (add-hook 'after-save-hook #'ts:init-lsp-maybe nil 'local)
        (lsp-deferred)
        (remove-hook 'after-save-hook #'ts:init-lsp-maybe 'local)))))


(use-package js
  :ensure nil
  :interpreter "node"
  :hook (js-mode . rainbow-delimiters-mode)
  :init
  ;; Parse node stack traces in the compilation buffer
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist 'node)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                        2 3 4)))
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  (js-chain-indent t))

(use-package typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode)
  :custom
  (typescript-indent-level 2)
  :config
  (setq-mode-local typescript-mode
                   comment-line-break-function #'c-indent-new-comment-line))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.l?eex\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :mode "\\.vue\\'"
  :hook ((web-mode . sgml-electric-tag-pair-mode)
         (web-mode-local-vars . ts:init-lsp-maybe))
  :custom
  (web-mode-enable-html-entities-fontification t)
  (web-mode-auto-close-style 1)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-pairing t)
  :config
  ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
  ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
  ;;    better.
  ;; 2. Strips out extra closing pairs to prevent redundant characters
  ;;    inserted by smartparens.
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
            (cl-loop for pair in (cdr alist)
                     unless (string-match-p "^[a-z-]" (cdr pair))
                     collect (cons (car pair)
                                   (string-trim-right (cdr pair)
                                                      "\\(?:>\\|]\\|}\\)+\\'"))))))

(use-package emmet-mode
  :hook (css-mode web-mode html-mode haml-mode nxml-mode)
  :config
  (when (package-installed-p 'yasnippet)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t))

;;;;; LSP and DAP
;;;;;

(use-package lsp-mode
  :commands (lsp-format-buffer
             lsp-organize-imports
             lsp-install-server)
  :hook
  (lsp-completion-mode . lsp:setup-completion)
  :preface
  (defvar lsp:defer-shutdown 3)

  (defun lsp:orderless-dispatch-flex-1st (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun lsp:setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless partial-completion)))
  :init
  (general-setq lsp-session-file (concat my-etc-dir "lsp-seesion")
                lsp-server-install-dir (concat my-etc-dir "lsp/")
                lsp-use-plists t)

  (add-hook 'orderless-style-dispatchers #'lsp:orderless-dispatch-flex-1st
            nil
            'local)

  ;; Make ‘lsp-completion-at-point’ nonexclusive
  (advice-add #'lsp-completion-at-point :around
    (lambda (orig-fn)
      (cape-wrap-properties orig-fn :exclusive 'no)))

  (setq-local completion-at-point-functions (list
                                             (cape-capf-buster
                                              (cape-super-capf
                                               #'lsp-completion-at-point
                                               #'cape-keyword
                                               #'cape-abbrev
                                               #'cape-dabbrev))
                                             #'cape-file))
  :custom
  (lsp-completion-provider :none) ; we use Corfu instead
  (lsp-diagnostics-provider :flymake)
  (lsp-enable-snippet nil)
  (lsp-keep-workspace-alive nil)
  (lsp-intelephense-storage-path (concat my-cache-dir "lsp-intelephense/"))
  (lsp-clients-emmy-lua-jar-path (concat lsp-server-install-dir "EmmyLua-LS-all.jar"))
  (lsp-xml-jar-file              (concat lsp-server-install-dir "org.eclipse.lsp4xml-0.3.0-uber.jar"))
  (lsp-groovy-server-file        (concat lsp-server-install-dir "groovy-language-server-all.jar"))
  (lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix nil)
  :config
  (defadvice! lsp:respect-user-defined-checkers (orig-fn &rest args)
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply orig-fn args)
          (setq-local flycheck-checker old-checker))
      (apply orig-fn args)))

  (defvar lsp:deferred-shutdown-timer nil)
  (defadvice! lsp:defer-server-shutdown (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
  This gives the user a chance to open other project files before the server is
  auto-killed (which is a potentially expensive process). It also prevents the
  server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null lsp:defer-shutdown)
            (= lsp:defer-shutdown 0))
        (funcall orig-fn restart)
      (when (timerp lsp:deferred-shutdown-timer)
        (cancel-timer lsp:deferred-shutdown-timer))
      (setq lsp:deferred-shutdown-timer
            (run-at-time
             (if (numberp lsp:defer-shutdown) lsp:defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall orig-fn)))))
             lsp--cur-workspace))))

  (add-hook! 'lsp-mode-hook
    (defun lsp:display-guessed-project-root ()
      "Log what LSP things is the root of the current project."
      ;; Makes it easier to detect root resolution issues.
      (when-let (path (buffer-file-name (buffer-base-buffer)))
        (if-let (root (lsp--calculate-root (lsp-session) path))
            (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
          (lsp--info "Could not guess project root."))))))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-max-height 8)
  (lsp-ui-doc-max-width 72)
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse nil)  ; don't disappear on mouseover
  (lsp-ui-doc-position 'at-point)
  ;; Don't show symbol definitions in the sideline. They are pretty noisy,
  ;; and there is a bug preventing Flycheck errors from being shown (the
  ;; errors flash briefly and then disappear).
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

(use-package dap-mode
  :when (package-installed-p 'lsp-mode)
  :after lsp-mode
  :hook ((dap-mode . dap-tooltip-mode)
         (dap-stopped . (lambda (_arg) (call-interactively #'dap-hydra))))
  :init
  (with-eval-after-load 'lsp
    (require 'dap-mode))
  :custom
  (dap-breakpoints-file (concat my-etc-dir "dap-breakpoints"))
  (dap-utils-extension-path (concat my-etc-dir "dap-extension/"))
  :config
  (require 'dap-ui)
  (add-hook 'dap-mode-hook #'dap-ui-mode)
  (add-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)
  ;; :config
  ;; TODO maybe need to make use of `dap-register-debug-template'?
  ;; (eval-after-load 'nim-mode
  ;;   (require 'dap-gdb-lldb))
  )

;;;;; Containers all the way down
;;;;;

(use-package dockerfile-mode
  :config
  (add-hook 'dockerfile-mode-local-vars-hook #'lsp-deferred))

(use-package docker
  :defer t)

(provide 'programming)
;;; programming.el ends here
