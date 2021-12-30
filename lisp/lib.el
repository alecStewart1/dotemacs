;;; lib.el --- Set of variables, constants, functions and macros -*- lexical-binding: t; -*-
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
;;
;;
;;; Code:

;;(require 'dash)
(require 'abbrev)

;;; Useful variables and constants
;;;

;;;###autoload
(defconst emacs27-p    (> emacs-major-version 26))
;;;###autoload
(defconst emacs28-p    (> emacs-major-version 27))
;;;###autoload
(defconst nativecomp-p (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
;;;###autoload
(defconst linux-p      (eq system-type 'gnu/linux))
;;;###autoload
(defconst macos-p      (eq system-type 'darwin))
;;;###autoload
(defconst bsd-p        (or macos-p (eq system-type 'berkeley-unix)))
;;;###autoload
(defconst cygwin-p     (eq system-type 'cygwin))
;;;###autoload
(defconst windows-nt-p (memq system-type '(cygwin windows-nt ms-dos)))

;; We actually need this here
(when (and windows-nt-p (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

(defconst my-emacs-dir (eval-when-compile (file-truename user-emacs-directory)))

(defconst my-local-dir (concat my-emacs-dir ".local/"))

(defconst my-etc-dir (concat (expand-file-name my-local-dir) "etc/"))

(defconst my-cache-dir (concat my-local-dir "cache/"))

(defvar transient-counter 0)

;;; The Emacs "Leader" Key and Keymap
;;;

;;;###autoload
(defvar ctrl-c-map (make-sparse-keymap)
  "The keymap for generic key combinations prefixed with Ctrl-c.")

(with-eval-after-load 'general 
  (general-create-definer emacs:leader-def
    :prefix "C-c"
    :keymap 'ctrl-c-map))

;;; Errors

;(define-error 'my-config-error "Error with my config")
;(define-error 'my-hook-error "An error with a hook of mine" 'my-config-error)

;;; Functions
;;;

(defun unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

;;;###autoload
(defun simple-call-process (command &rest args)
  "From Doom Emacs.
Execute COMMAND with ARGS synchronously.
Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))


;;;###autoload
(defun get-buffer-mode (buf)
  "Get `major-mode' of BUF."
  (with-current-buffer buf
    major-mode))

;;;###autoload
(defun buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (buffer-list))))

;;;###autoload
(defun visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

;;;###autoload
(defvar fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

;;;###autoload
(defun fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create fallback-buffer-name)))


(defun resolve-hook-forms (hooks)
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (enlist (unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (loop for hook in hook-list
            if (eq (car-safe hook) 'quote)
            collect (cadr hook)
            else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun setq-hook--fns (hooks rest &optional singles)
  (cl-declare (optimize (speed 3) (safety 3)))
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (loop with vars = (let ((args rest)
                          vars)
                      (while args
                        (push (if singles
                                  (list (pop args))
                                (cons (pop args) (pop args)))
                              vars))
                      (nreverse vars))
        for hook in (resolve-hook-forms hooks)
        for mode = (string-remove-suffix "-hook" (symbol-name hook))
        append
        (loop for (var . val) in vars
              collect (list var val hook
                            (intern (format "setq-%s-for-%s-hook"
                                            var mode))))))

;;;###autoload
(defun resolve-file-path-forms (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.
For example
  (resolve-file-path-forms
    '(or A (and B C))
    \"~\")
Returns (approximately):
  '(let* ((_directory \"~\")
          (A (expand-file-name A _directory))
          (B (expand-file-name B _directory))
          (C (expand-file-name C _directory)))
     (or (and (file-exists-p A) A)
         (and (if (file-exists-p B) B)
              (if (file-exists-p C) C))))
This is used by `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (if (and (listp spec)
           (memq (car spec) '(or and)))
      (cons (car spec)
            (mapcar (rpartial #'resolve-file-path-forms directory)
                    (cdr spec)))
    (let ((filevar (make-symbol "file")))
      `(let ((,filevar ,spec))
         (and (stringp ,filevar)
              ,(if directory
                   `(let ((default-directory ,directory))
                      (file-exists-p ,filevar))
                 `(file-exists-p ,filevar))
              ,filevar)))))


;;;###autoload
(defmacro file-exists-p! (files &optional directory)
  "Returns non-nil if the FILES in DIRECTORY all exist.

DIRECTORY is a path; defaults to `default-directory'.

Returns the last file found to meet the rules set by FILES, which can be a
single file or nested compound statement of `and' and `or' statements."
  `(let ((p ,(resolve-file-path-forms files directory)))
     (and p (expand-file-name p ,directory))))

;;;; Stuff for certain packages that a lot of configurations need
;;;;

;;;;; Projectile
;;;;;

;;;###autoload
(defun projectile:get-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;;; Skeleton
;;;;;

;; Thank you reddit user b3n:
;; https://old.reddit.com/r/emacs/comments/ml4wql/weekly_tipstricketc_thread/gtkc524/
;;;###autoload
(defmacro global-snippet (name &rest skeleton)
  "Create a global \"snippet\" with NAME and SKELETON.
NAME must be valid in the Emacs Lisp naming convention.

SKELETON must be a body that is valid to `Skeleton''s internal language.

This macro makes use of `define-skeleton' and `define-abbrev' in order to
create something similar to a code/writing snippet system, like that of
`YASnippet'. Keep in mind that all abbreviations created are put in the
`global-abbrev-table' under the named passed to this macro. That may or
may not be something you want, depending on your uses.
If you're looking to only define an abbrev for a specific mode, see
`mode-snippet’."
  (declare (debug t)
           (indent defun))
  (let* ((snip-name (symbol-name `,name))
         (func-name (intern (concat snip-name "-skel"))))
    `(progn
       (define-skeleton ,func-name
         ,(concat snip-name " skeleton")
         ,@skeleton)
       (define-abbrev global-abbrev-table ,snip-name
         "" ',func-name))))

;;;###autoload
(defmacro mode-snippet (name mode &rest skeleton)
  "Create a MODES specific \"snippet\" with NAME and SKELETON.
NAME must be valid in the Emacs Lisp naming convention.

MODE must be a valid major or minor mode that

SKELETON must be a body that is valid to `Skeleton''s internal language.
This macro makes use of `define-skeleton' and `define-abbrev' in order to
create something similar to a code/writing snippet system, like that of
`YASnippet'.

Keep in mind that all abbreviations created are put in the abbrev table of
MODE you passed to this macro. That may or may not be something you want,
depending on your uses. If you're looking to only define an abbrev globally,
see `snippets:global-snip'."
  (declare (debug t)
           (indent defun))
  ;; TODO need to figure out how to work with lists better
  (let* ((snip-name (symbol-name `,name))
         (func-name (intern (concat snip-name "-skel")))
         (var-str (concat (symbol-name mode) "-abbrev-table"))
         (abbrev-table (intern-soft var-str)))
    `(progn
       (define-skeleton ,func-name
          ,(format "%s %s %s %s." snip-name "skeleton. Defined in" var-str "abbreviaton table.")
          ,@skeleton)
       (define-abbrev ,abbrev-table ,snip-name
         "" ',func-name))))

;; (define-abbrev emacs-lisp-mode-abbrev-table "defun" ""
;;   'emacs-lisp-defun-skel)

;; (define-skeleton emacs-lisp-defun-skel
;;   "Create a emacs defun at point"
;;   "Function name: "
;;   > "(defun " str " ()" ?\n
;;   > _ ?\n
;;   > ")")

;;; Macros
;;;

;;;###autoload
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "From Doom Emacs.
Attaches a self-removing function to HOOK-OR-FUNCTION.
FORMS are evaluated once, when that function/hook is first invoked, then never
again.
HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "transient-%d-h" (cl-incf transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

;;;###autoload
(defmacro add-hook-trigger! (hook-var &rest targets)
  "TODO"
  `(let ((fn (intern (format "%s-h" ,hook-var))))
     (fset fn (lambda (&rest _) (run-hooks ,hook-var) (set ,hook-var nil)))
     (put ,hook-var 'permanent-local t)
     (dolist (on (list ,@targets))
       (if (functionp on)
           (advice-add on :before fn)
         (add-hook on fn)))))

;;;###autoload
(defmacro add-hook! (hooks &rest rest)
  "From Doom Emacs.

A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         depth
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'unquote rest)
                     (enlist (unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,(or depth append-p) ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

;;;###autoload
(defmacro setq-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent 1))
  (macroexp-progn
   (loop for (var val hook fn) in (setq-hook--fns hooks var-vals)
         collect `(defun ,fn (&rest _)
                    ,(format "%s = %s" var (pp-to-string val))
                    (setq-local ,var ,val))
         collect `(remove-hook ',hook #',fn)
         collect `(add-hook ',hook #',fn))))

;;;###autoload
(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (loop for (_var _val hook fn)
         in (setq-hook--fns hooks vars 'singles)
         collect `(remove-hook ',hook #',fn))))

;;;###autoload
(defmacro letf! (bindings &rest body)
  "From Doom Emacs
Temporarily rebind function and macros in BODY.
BINDINGS is either a) a list of, or a single, `defun' or `defmacro'-ish form, or
b) a list of (PLACE VALUE) bindings as `cl-letf*' would accept.
TYPE is either `defun' or `defmacro'. NAME is the name of the function. If an
original definition for NAME exists, it can be accessed as a lexical variable by
the same name, for use with `funcall' or `apply'. ARGLIST and BODY are as in
`defun'.
\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defmacro))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) (macroexpand body))
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defun `(cl-letf* ((,(car rest) (symbol-function #',(car rest)))
                                  ((symbol-function #',(car rest))
                                   (lambda ,(cadr rest) ,@(cddr rest))))
                         (ignore ,(car rest))
                         ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

;;;###autoload
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

;;;###autoload
(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.
This can be used as a drop-in replacement for `setq'. Particularly when you know
a variable has a custom setter (a :set property in its `defcustom' declaration).
This triggers setters. `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

;;;###autoload
(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.
If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

;;;###autoload
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

;;;###autoload
(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

;;;###autoload
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "From Doom Emacs.

Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

;;;###autoload
(defmacro defer-feature! (feature &rest fns)
  "From Doom Emacs.

Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN runs.
Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if FN is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "doom--defer-feature-%s-a" feature))))
    `(progn
       (delq! ',feature features)
       (defadvice! ,advice-fn (&rest _)
         :before ',fns
         ;; Some plugins (like yasnippet) will invoke a fn early to parse
         ;; code, which would prematurely trigger this. In those cases, well
         ;; behaved plugins will use `delay-mode-hooks', which we can check for:
         (unless delay-mode-hooks
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (dolist (fn ',fns)
             (advice-remove fn #',advice-fn)))))))

;;;###autoload
(defmacro shut-up! (&rest forms)
  "From Doom Emacs’s ‘quiet!’.

Runs FORMS without generating any output.

Emacs likes to tell you _everything_. Tell it not to sometimes.
This won’t prevent the *Messages* buffer from being wrote to, just in the echo area."
  `(unless noninteractive
     (let ((inhibit-message t)
           (save-silently t))
       (prog1 ,@forms (message "")))
     (letf! ((standard-output (lambda (&rest _)))
             (defun message (&rest _))
             (defun load (file &optional noerror nomessage nosuffix must-suffix)
               (funcall load file noerror t nosuffix must-suffix))
             (defun write-region (start end filename &optional append visit lockname mustbenew)
               (unless visit (setq visit 'no-message))
               (funcall write-region start end filename append visit lockname mustbenew)))
       ,@forms)))

(provide 'lib)
;;; lib.el ends here
