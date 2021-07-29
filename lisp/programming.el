;;; programming.el --- Configurations for programming -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: January 23, 2021
;; Modified: January 23, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Packages that I use for programming, as well as some code
;;  I use to format said code.
;;
;;  TODO - add in Polymode?
;;;
;;; Code:

(require 'lib)
(require 'cl-lib)
(require 'cl-seq)
(require 'subr-x)
(require 'mode-local)

;;; Personal Code
;;;

;;;; Formatting Code
;;;; TODO - format region

;;;###autoload
(defvar format:formatter-command-alist '((cs   . "clang-format")
                                         (py   . "black")
                                         (nim  . "nimpretty")
                                         (sh   . "shfmt")
                                         (fish . "fish_indent")
                                         (md   . "prettier")
                                         (html . "prettier")
                                         (js   . "prettier")
                                         (css  . "prettier")
                                         (scss . "prettier")
                                         (json . "prettier")
                                         (yaml . "prettier")))

;;;###autoload
(defvar format:formatter-args-alist '((cs . " --sort-includes")
                                      (fish . " -w")
                                      (md   . " -w --prose-wrap --parser markdown")
                                      (html . " -w --parser html")
                                      (css  . " -w --parser css")
                                      (scss . " -w --parser scss")
                                      (json . " -w --parser json")
                                      (yaml . " -w --parser yaml")))
;; TODO
;;;###autoload
(defvar format:formatter-region-args-alist '((py   . " -c")))

(defun format::shfmt-lang-args ()
  (let ((lang " -ln "))
    (cl-case (and (eql major-mode 'sh-mode)
                  (boundp 'sh-shell)
                  (symbol-value 'sh-shell))
      (bash (concat lang "bash"  " -s"))
      (mksh (concat lang "mksh"  " -s"))
      (t    (concat lang "posix" " -s")))))

(defun format:get-command-for-mode (mode &optional on-region-p)
  "Get the commandline command for formatting for the current MODE.
ON-REGION-P (not yet implemented) when non-nil is for when we want to only
format text in a specific region."
  (let ((get-cmd (lambda (k)
                   (alist-get k 'format:formatter-command-alist)))
        (get-args (lambda (k)
                    (if on-region-p
                        (alist-get k 'format:formatter-region-args-alist)
                      (alist-get k 'format:formatter-args-alist)))))
    (pcase mode
      ((or c-mode c++-mode objc-mode)  (concat (get-cmd 'cs) (get-args 'cs)))
      (python-mode                     (get-cmd 'py))
      (nim-mode                        (concat (get-cmd 'nim) (get-args 'nim)))
      (sh-mode                         (concat (get-cmd 'sh) (format::shfmt-lang-args)))
      (fish-mode                       (concat (get-cmd 'fish) (get-args 'fish)))
      (markdown-mode                   (concat (get-cmd 'md) (get-args 'md)))
      ((or html-mode mhtml-mode)       (concat (get-cmd 'html) (get-args 'html)))
      ((or js-mode js2-mode rjsx-mode) (get-cmd 'js))
      (css-mode                        (concat (get-cmd 'css) (get-args 'css)))
      (scss-mode                       (concat (get-cmd 'scss) (get-args 'scss)))
      (json-mode                       (concat (get-cmd 'json) (get-args 'json)))
      (yaml-mode                       (concat (get-cmd 'yaml) (get-args 'yaml)))
      (_                               #'indent-region))))

(defun format:format-current-file ()
  "Format the current file in the current buffer."
  (interactive)
  (let* ((mode    (with-current-buffer (current-buffer) major-mode))
         (cmd     (formet:get-command-for-mode mode))
         (file    (expand-file-name (buffer-file-name))))
    (if (stringp cmd)
        ;; TODO Is this jank?
        (async-shell-command
         (concat cmd " " file)
         nil t)
      ;; If we don't have a formatter for the given langauge, we'll
      ;; end up using `indent-region'
      (save-excursion
        (mark-whole-buffer)
        (funcall-interactively cmd (point-min) (point-max))))))

;; TODO
;; (defun format:format-region ()
;;   (interactive)
;;   (let* ((mode (with-current-buffer (current-buffer) major-mode))
;;          (cmd  (format:get-command-for-mode mode)))
;;     (if (stringp cmd)
;;         (shell-command-on-region
;;          (region-beginning) (region-end) cmd nil t nil t nil))))

;;; Packages
;;;

;;;; Utilities
;;;;

(use-package compile
  :hook (compilation-filter . compile:colorize-compilation-buffer)
  :preface
  (defun compile:colorize-compilation-buffer ()
    "ANSI coloring in the compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :custom
  (compilation-always-kill . 1)
  (compilation-ask-about-save . nil)
  (compilation-scroll-output . 'first-error))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package goto-addr
  :hook (prog-mode . goto-address-mode))

(use-package eldoc
  :diminish)

(use-package rmsbolt
  :after (:any c-mode c++-mode objc-mode emacs-lisp-mode common-lisp-mode))

(use-package tree-sitter
  :if (functionp 'module-load)
  :hook ((sh-mode . tree-sitter-mode)
         (c-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (csharp-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (c++-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (css-mode . tree-sitter-mode)
         (html-mode . tree-sitter-mode)
         (mhtml-mode . tree-sitter-mode)
         (java-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (js-mode . tree-sitter-mode)
         (js-jsx-mode . tree-sitter-mode)
         (js2-mode . tree-sitter-mode)
         (ruby-mode  . (tree-sitter-mode tree-sitter-hl-mode))
         (enh-ruby-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (rjsx-mode . tree-sitter-mode)
         (typescript-mode . tree-sitter-mode)
         (json-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (jsonc-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (python-mode . (tree-sitter-mode tree-sitter-hl-mode))
         (ruby-mode . (tree-sitter-mode tree-sitter-hl-mode))))

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

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :preface
  (defun projectile:cleanup-project-cache ()
    "Purge projectile cache entries that:

    a) have too many files (see `projectile:cache-limit'),
    b) represent blacklisted directories that are too big, change too often or are
       private. (see `projectile:cache-blacklist'),
    c) are not valid projectile projects."
    (when (and (bound-and-true-p projectile-projects-cache)
               projectile-enable-caching)
      (setq projectile-known-projects
            (cl-remove-if #'projectile-ignored-project-p
                          projectile-known-projects))
      (projectile-cleanup-known-projects)
      (cl-loop with blacklist = (mapcar #'file-truename projectile:cache-blacklist)
               for proot in (hash-table-keys projectile-projects-cache)
               if (or (not (stringp proot))
                      (>= (length (gethash proot projectile-projects-cache))
                          projectile:cache-limit)
                      (member (substring proot 0 -1) blacklist)
                      (and projectile:cache-purge-non-projects
                           (not (projectile-project-p proot)))
                      (projectile-ignored-project-p proot))
               do (doom-log "Removed %S from projectile cache" proot)
               and do (remhash proot projectile-projects-cache)
               and do (remhash proot projectile-projects-cache-time)
               and do (remhash proot projectile-project-type-cache))
      (projectile-serialize-cache)))

  :init
  (defvar projectile:cache-limit 10000)
  (defvar projectile:cache-blacklist '("~" "/tmp" "/"))
  (defvar projectile:cache-purge-non-projects nil)
  (defvar projectile:fd-binary (cl-find-if #'executable-find (list "fdfind" "fd")))
  :custom
  (projectile-cache-file (concat my-cache-dir "projectile.cache"))
  (projectile-auto-discover nil)
  ;;(projectile-enable-caching doom-interactive-p)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-known-projects-file (concat my-cache-dir "projectile.projects"))
  (projectile-ignored-projects (list "~/" temporary-file-directory))
  (projectile-project-root-files-bottom-up
                                           (append '(".projectile"  ; projectile's root marker
                                                     ".project"     ; doom project marker
                                                     ".git")        ; Git VCS root dir
                                                   (when (executable-find "hg")
                                                     '(".hg"))      ; Mercurial VCS root dir
                                                   (when (executable-find "bzr")
                                                     '(".bzr"))))  ; Bazaar VCS root dir
  (projectile-project-root-files '())
  (projectile-project-root-files-top-down-recurring '("Makefile"))
  (projectile-git-submodule-command nil)
  (projectile-indexing-method 'hybrid)
  (projectile-generic-command
   (lambda (_)
     (cond ((when-let (bin (if (ignore-errors (file-remote-p default-directory nil t))
                               (cl-find-if (-rpartial #'executable-find t)
                                           (list "fdfind" "fd"))
                             projectile:fd-binary))
              (concat (format "%s . -0 -H --color=never --type file --type symlink --follow --exclude .git" bin)
                      (if windows-nt-p "--path-separator=/"))))
           ((executable-find "rg" t)
            (concat "rg -0 --files --follow --color=never --hidden -g!.git"
                    (if window-nt-p "--path-separator=/")))
           ("find . -type f -print0"))))
  :config
  (projectile-mode +1)

  (add-transient-hook! 'projectile-relevant-known-projects
    (projectile-cleanup-known-projects)
    (projectile-discover-projects-in-search-path))

  (push (abbreviate-file-name my-local-dir) projectile-globally-ignored-directories)

  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  (defadvice! projectile:dirconfig-file ()
    :override #'projectile-dirconfig-file
    (cond ((file-exists-p! (or ".projectile" ".project") (projectile-project-root)))
          ((expand-file-name ".project" (projectile-project-root)))))

  (put 'projectile-ag 'disabled "Use +{vertico,selectrum}/project-search instead")
  (put 'projectile-ripgrep 'disabled "Use +{vertico,seletrum}/project-search instead")
  (put 'projectile-grep 'disabled "Use +{vertico,seletrum}/project-search instead")

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (add-hook! 'kill-emacs-hook #'projectile:cleanup-project-cache)

  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  (defadvice! projectile:only-use-generic-command (vcs)
    "Only use `projectile-generic-command' for indexing project files.
And if it's a function, evaluate it."
    :override #'projectile-get-ext-command
    (if (functionp projectile-generic-command)
        (funcall projectile-generic-command vcs)
      projectile-generic-command))

  (defadvice! projectile:default-generic-command (orig-fn &rest args)
    :around #'projectile-default-generic-command
    (ignore-errors (apply orig-fn args)))

  ;; It breaks projectile's project root resolution if HOME is a project (e.g.
  ;; it's a git repo). In that case, we disable bottom-up root searching to
  ;; prevent issues. This makes project resolution a little slower and less
  ;; accurate in some cases.
  (let ((default-directory "~"))
    (when (cl-find-if #'projectile-file-exists-p
                      projectile-project-root-files-bottom-up)
      ;(doom-log "HOME appears to be a project. Disabling bottom-up root search.")
      (setq projectile-project-root-files
            (append projectile-project-root-files-bottom-up
                    projectile-project-root-files)
            projectile-project-root-files-bottom-up nil))))

;;;; Assembler
;;;;

(use-package asm-mode
  :mode "\\.inc$")

(use-package nasm-mode
  :mode "\\.nasm$")

(use-package mips-mode
  :mode "\\.mips$")

(use-package masm-mode
  :if (or windows-nt-p cygwin-p)
  :mode "\\.masm$")

;;;; C/C++, Objective-C
;;;;

(use-package cc-mode
  :mode ("\\.mm\\'" . objc-mode)
  :hook ((c-mode-local-vars
          c++-mode-local-vars
          objc-mode-local-vars
          cmake-mode-local-vars) . lsp-deferred)
  :hook (c-mode-common . rainbow-delimiters-mode)
  :custom
  (c-default-style . "bsd")
  (c-basic-offset . tab-width)
  (c-backspace-function . #'delete-backward-char))

;; (use-package ccls
;;   :when (package-installed-p 'lsp-mode)
;;   :hook (lsp-lens-mode . ccls-code-lens-mode)
;;   :preface
;;   (defvar ccls-sem-highlight-method 'tree-sitter-hl-mode)
;;   :init
;;   (with-eval-after-load 'projectile
;;     (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
;;     (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
;;     (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
;;   (with-eval-after-load 'lsp-mode
;;     (require 'ccls))
;;   :config
;;   (setq-hook! 'lsp-configure-hook
;;     ccls-sem-highlight-method (if lsp-enable-semantic-highlighting
;;                                   ccls-sem-highlight-method))
;;   (when (or macos-p linux-p)
;;     (setq ccls-initialization-options
;;           `(:index (:trackDependency 1)))))

;;;; Shell
;;;;

(use-package sh-script
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

  (add-to-list 'sh-imenu-generic-expression
               'sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
               (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1))
  (add-hook! 'sh-mode-hook
    (defun sh-init-extra-fontification ()
      (font-lock-add-keywords nil
                              `((sh--match-variables-in-quotes
                                 (1 'font-lock-constant-face preprend)
                                 (2 'font-lock-variable-name-face prepend))
                                (sh--match-command-subst-in-quotes
                                 (1 'sh-quoted-exec prepend))
                                (,(regexp-opt sh-builtin-keywords 'symbols)
                                 (0 'font-lock-type-face append))))))
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode)
  (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p)))

(use-package company-shell
  :after sh-script
  :config
  (company:set-backend 'sh-mode '(company-shell company-files))
  (setq company-shell-delete-duplicates t))

(use-package fish-mode)

;;;; Emacs-Lisp
;;;;

(use-package elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :hook (before-save . format:format-current-file)
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
  (tab-width . 8)
  (mode-name . "Elisp")
  (outline-regexp . "[ \t];;;; [^ \t\n]")
  (lisp-indent-function . #'elisp-mode:indent-function)
  :config
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
  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  :config
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

;;;; Common Lisp
;;;;

;; (defer-feature! lisp-mode)

;; (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

;; (use-package sly
;;   :hook (lisp-mode-local-vars-hook . sly-editing-mode)
;;   :preface
;;   (defvar inferior-lisp-program "sbcl")
;;   :init
;;   (add-hook! 'after-init-hook
;;     (with-eval-after-load 'sly
;;       (sly-setup)))
;;   :custom
;;   (sly-mrepl-history-file-name (concat my-cache-dir "sly-mrepl-history"))
;;   (sly-net-coding-system 'utf-8-unix)
;;   (sly-kill-without-query-p t)
;;   (sly-lisp-implementations
;;    `((sbcl (,(executable-find "sbcl")))
;;      (ccl (,(executable-find "ccl")))
;;      (ecl (,(executable-find "ecl")))
;;      (abcl (,(executable-find "abcl")))
;;      (clisp (,(executable-find "clisp")))))
;;   (sly-description-autofocus t)
;;   (sly-inhibit-pipelining nil)
;;   (sly-load-failed-fasl 'always)
;;   (sly-complete-symbol-function 'sly-flex-completions)
;;   :config
;;   (defun sly:cleanup-maybe ()
;;     "Kill processes and leftover buffers when killing the last sly buffer."
;;     (let ((buf-list (delq (current-buffer) (buffer-list))))
;;       (unless (cl-loop for buf in buf-list
;;                        if (and (buffer-local-value 'sly-mode buf)
;;                                (get-buffer-window buf))
;;                        return t)
;;         (dolist (conn (sly--purge-connections))
;;           (sly-quit-list-internal conn 'sly-quit-sentinel t))
;;         (let (kill-buffer-hook kill-buffer-query-functions)
;;           (mapc #'kill-buffer
;;                 (cl-loop for buf in buf-list
;;                          if (buffer-local-value 'sly-mode buf)
;;                          collect buf))))))

;;   (defun sly:init ()
;;     "Attempt to auto-start sly when opening a lisp buffer."
;;     (cl-labels ((temp-buf-p (buf)
;;                             (equal (substring (buffer-name buf) 0 1) " ")))
;;       (cond ((or (temp-buf-p (current-buffer))
;;                  (sly-connected-p)))
;;             ((executable-find interfior-lisp-program)
;;              (let ((sly-auto-start 'always))
;;                (sly-auto-start)
;;                (add-hook 'kill-buffer-hook #'sly:cleanup-maybe nil t)))
;;             ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
;;                       inferior-lisp-program)))))
;;   (add-hook! 'sly-mode-hook #'sly:init))

;; (use-package sly-repl-ansi-color
;;   :after sly
;;   :init
;;   (add-to-list 'sly-contribs 'sly-repl-ansi-color))

;; (use-package sly-asdf
;;   :after sly
;;   :config
;;   (add-to-list 'sly-contribs 'sly-asdf #'append)
;;   (with-eval-after-load 'sly
;;     (sly-enable-contrib 'sly-asdf)))

;; (use-package sly-quicklisp
;;   :after sly
;;   :commands sly-quicklisp)

;; (use-package sly-named-readtables
;;   :after sly
;;   :config
;;   (with-eval-after-load 'sly
;;     (sly-enable-contrib 'sly-named-readtables)))

;;;; Schemes
;;;;

(use-package scheme
  :hook (scheme-mode . rainbow-delimiters-mode)
  :preface
  (defvar calculate-lisp-indent-last-sexp)
  :config
  (defadvice! scheme:indent-function (indent-point state)
    "A better indenting function for `scheme-mode'."
    :override #'scheme-indent-function
    (let ((normal-indent (current-column)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indnet-last-sexp 0 t)
      (if (and (elt state 2)
               (not (looking-at-p "\\sw\\|\\s_")))
          (progn
            (unless (> (save-excursion (forward-line 1) (point))
                       calculate-lisp-indent-last-sexp)
              (goto-char calculate-lisp-indent-last-sexp)
              (beginning-of-line)
              (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
            (backward-prefix-chars)
            (current-column))
        (let* ((function (buffer-substring
                          (point)
                          (progn
                            (forward-sexp 1)
                            (point))))
               (method (or (get (intern-soft function) 'scheme-indent-function)
                           (get (intern-soft function) 'scheme-indent-hook))))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match-p "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((and (null method)
                      (> (length function) 1)
                      (string-match-p "\\`:" function))
                 (let ((lisp-body-indent 1))
                   (lisp-indent-defform state indent-point)))
                ((integerp method)
                 (lisp-indent-specform method state indent-point normal-indent))
                (method
                 (funcall method state indent-point normal-indent))))))))

(use-package geiser
  :defer t
  :preface
  (defun geiser/open-repl ()
    "Open the Geiser REPL."
    (interactive)
    (call-interactively #'switch-to-geiser)
    (current-buffer))
  :init
  ;; To work with Guile + Geiser
  ;; We need this first in order to set our `geiser-activate-implementation' variable
  (use-package geiser-guile :ensure t)
  :custom
  ;; May work with Gambit/Gerbil, I dunno.
  ;; Guile, Chez, and Chicken are the better documented at the moment.
  (geiser-active-implementations . '(guile chez chicken))
  (geiser-autodoc-identifier-format . "%s => %s")
  (geiser-repl-current-project-function . 'projectile-project-root))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook (racket-mode-local-vars . (racket-xp-mode lsp-deferred))
  :preface
  (defun racket:open-repl ()
    "Open the Racket REPL."
    (interactive)
    (pop-to-buffer
     (or (get-buffer "*Racket REPL*")
         (progn (racket-run-and-switch-to-repl)
                (let ((buf (get-buffer "*Racket REPL*")))
                  (bury-buffer buf)
                  buf)))))
  :config
  (require 'smartparens-racket)
  (add-hook! 'racket-mode-hook
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode)

  (add-hook! 'racket-xp-mode-hook
    (defun racket-xp-disable-flycheck ()
      (cl-pushnew 'racket flyheck-disabled-checkers))))

;;;; .NET Core
;;;;

;; (with-eval-after-load 'projectile
;;   (pushnew! projectile-project-root-files "global.json")

;;   (defun projectile:dotnet-project-p ()
;;     (or (projectile-verify-file-wildcard "?*.csproj")
;;         (projectile-verify-file-wildcard "?*.fsproj")
;;         (projectile-verify-file-wildcard "?*.sln")))

;;   (projectile-register-project-type 'dotnet #'projectile:dotnet-project-p
;;                                     :compile "dotnet build"
;;                                     :run "dotnet run"
;;                                     :test "dotnet test"))

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


;;;; Go
;;;;

;; (use-package go-mode
;;   :defer t
;;   :config
;;   (add-hook 'go-mode-local-vars-hook #'lsp-deferred))

;; (use-package gorepl-mode
;;   :commands gorepl-run-load-current-file)

;; (use-package go-gen-test
;;   :after go-mode)

;;;; ESS
;;;;
;;;; I mostly use this for Julia support

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

;;;;; For the LSP support for Julia
;;;;;

;; (use-package lsp-julia
;;   :after lsp-mode
;;   :custom
;;   (lsp-julia-default-environment "~/.julia/environment/v1.0"))

;;;; Nim
;;;;

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
                  (replace-regexp-in-string "[꞉* |<>\"?*]" "" path)))))

(use-package ob-nim
  :after ob)

;;;; Erlang
;;;;

;; (use-package erlang
;;   :mode ("\\.erlang\\'" . erlang-mode)
;;   :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
;;   :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
;;   :config
;;   (when (package-installed-p 'lsp-mode)
;;     (add-hook 'erlang-mode-local-vars-hook #'lsp-deferred)))

;;;; Elixir
;;;;

(use-package elixir-mode
  :defer t
  :init
  (provide 'smartparens-elixir)
  :config
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (when (package-installed-p 'lsp-mode)
    (add-hook 'elixir-mode-local-vars-hook #'lsp-deferred)
    (eval-after-load 'lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'"))
    (require 'dap-elixir))

  (eval-after-load 'highlight-numbers
    (puthash 'elixir-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist)))

(use-package mix
  :after elixir-mode
  :hook (elixir-mode . mix-minor-mode))

(use-package exunit
  :hook (elixir-mode . exunit-mode))

(use-package inf-elixir
  :general
  (:keymaps 'elixir-mode-map
   :prefix "C-c i"
   "i" #'inf-elixir
   "p" #'inf-elixir-project
   "l" #'inf-elixir-send-line
   "r" #'inf-elixir-send-region
   "b" #'inf-elixir-send-buffer))

(use-package ob-elixir
  :after ob)

;;;; Ruby
;;;;

(use-package enh-ruby-mode
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :config
  (add-hook 'enh-ruby-mode-local-vars-hook #'lsp-deferred)
  (setq-mode-local enh-ruby-mode sp-max-pair-length 6))

(use-package inf-ruby
  :after enh-ruby-mode
  :config
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

(use-package yard-mode
  :after enh-ruby-mode)

(use-package rake
  :after enh-ruby-mode
  :commands (rake rake-rerun rake-regenerate-cache rake-find-task)
  :custom
  (rake-cache-file (concat my-cache-dir "rake.cache"))
  (rake-completion-system 'default))

(use-package ruby-test-mode
  :after enh-ruby-mode)

(use-package projectile-rails
  :hook ((enh-ruby-mode inf-ruby-mode projectile-rails-server-mode) . projectile-rails-mode)
  :hook (projectile-rails-mode-hook . auto-insert-mode)
  :init
  (setq auto-insert-query nil)
  (setq inf-ruby-console-environment "development"))

;;;; Python
;;;;

;; (use-package python
;;   :ensure nil
;;   :mode ("[./]flake8\\'" . conf-mode)
;;   :mode ("/Pipfile\\'" . conf-mode)
;;   :hook (python-mode . lsp-deferred)
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

;;   (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
;;   (sp-local-pair 'python-mode "'" nil
;;                  :unless '(sp-point-before-word-p
;;                            sp-point-after-word-p
;;                            sp-point-before-same-p)))

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

;;;; Webdev
;;;;

(use-package nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.xaml\\'"
  :mode "\\.rss\\'"
  :magic "<\\?xml"
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t)
  :config
  (company:set-backend 'nxml-mode '(company-nxml)))

(use-package mhtml-mode
  :mode "\\.html?\\'")

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
  :init
  ;; If the user has installed `vue-mode' then, by appending this to
  ;; `auto-mode-alist' rather than prepending it, its autoload will have
  ;; priority over this one.
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
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
  :hook (css-mode-hook web-mode-hook html-mode-hook haml-mode-hook nxml-mode-hook)
  :config
  (when (package-installed-p 'yasnippet)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t))

;;;; LSP and DAP
;;;;

(use-package lsp-mode
  :commands (lsp-format-buffer
             lsp-organize-imports
             lsp-install-server)
  :hook
  (lsp-mode . (lambda ()
                     (add-hook 'before-save-hook #'lsp-format-buffer t t)
                     (add-hook 'before-save-hook #'lsp-organize-imports t t)))
  :preface
  (defvar lsp-company-backends 'company-capf)
  (defvar lsp--deferred-shutdown-timer nil)
  :init
  (setq lsp-session-file (concat my-etc-dir "lsp-seesion")
        lsp-server-install-dir (concat my-etc-dir "lsp/"))
  :custom
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
  (defadvice! lsp:respect-user-defuned-checkers (orig-fn &rest args)
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply orig-fn args)
          (setq-local flycheck-checker old-checker))
      (apply orig-fn args)))

  (defadvice! lsp:defer-server-shutdown (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
  This gives the user a chance to open other project files before the server is
  auto-killed (which is a potentially expensive process). It also prevents the
  server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null lsp-defer-shutdown)
            (= lsp-defer-shutdown 0))
        (funcall orig-fn restart)
      (when (timerp lsp--deferred-shutdown-timer)
        (cancel-timer lsp--deferred-shutdown-timer))
      (setq lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp lsp-defer-shutdown) lsp-defer-shutdown 3)
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
          (lsp--info "Could not guess project root.")))))

  (add-hook! 'lsp-completion-mode-hook
    (defun lsp:init-company-backends ()
      (when lsp-completion-mode
        (set (make-local-variable 'company-backends)
             (cons lsp-company-backends
                   (remove lsp-company-backends
                           (remq 'company-capf company-backends))))))))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-max-height 8)
  (lsp-ui-doc-max-width 35)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse nil)  ; don't disappear on mouseover
  (lsp-ui-doc-position 'at-point)
  ;; Don't show symbol definitions in the sideline. They are pretty noisy,
  ;; and there is a bug preventing Flycheck errors from being shown (the
  ;; errors flash briefly and then disappear).
  (lsp-ui-sideline-show-hover nil))

(use-package dap-mode
  :when (package-installed-p 'lsp-mode)
  :hook (dap-mode . dap-tooltip-mode)
  :init
  (with-eval-after-load 'lsp
    (require 'dap-mode))
  :custom
  (dap-breakpoints-file (concat my-etc-dir "dap-breakpoints"))
  (dap-utils-extension-path (concat my-etc-dir "dap-extension/"))
  :config
  (eval-after-load 'nim-mode
    (require 'dap-gdb-lldb)))

(use-package dap-ui
  :when (and (package-installed-p 'lsp-mode) (package-installed-p 'dap-mode))
  :hook ((dap-mode . dap-ui-mode)
         (dap-ui-mode . dap-ui-controls-mode)))

;;;; Docker
;;;;

(use-package dockerfile-mode
  :config
  (add-hook 'dockerfile-mode-local-vars-hook #'lsp-deferred))

(use-package docker
  :bind ("C-c d" . docker))

(provide 'programming)
;;; programming.el ends here
