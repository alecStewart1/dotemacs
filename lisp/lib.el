;;; lib.el --- Set of variables, constants, functions and macros -*- lexical-binding: t; -*-
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
;;
;;
;;; Code:

;;; Useful variables and constants
;;;

;;;###autoload
(defconst emacs27-p    (> emacs-major-version 26))
;;;###autoload
(defconst emacs28-p    (> emacs-major-version 27))
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

;;; Errors

(define-error 'my-config-error "Error with my config")
(define-error 'my-hook-error "An error with a hook of mine" 'my-config-error)

;;; Hooks
;;;

;;;###autoload
(defvar first-input-hook nil)
;;;###autoload
(defvar first-file-hook nil)
;;;###autoload
(defvar first-buffer-hook nil)
;;;###autoload
(defvar switch-buffer-hook nil)
;;;###autoload
(defvar inhibit-switch-buffer-hooks nil)
;;;###autoload
(defvar inhibit-local-var-hooks nil)

(defvar transient-counter 0)

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

;;;###autoload
(defun get-buffer-mode (buf)
  "Get `major-mode' of BUF."
  (with-current-buffer buf
    major-mode))

;;;###autoload
(defun buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (doom-enlist modes)))
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

(defun buffer-temp-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

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

;;;; Run hooks on a specific function or when switching a buffer.
;;;; We need these here for bootstraping the configuration

;;;###autoload
(defun try-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'my-hook-error (list hook e))))
  nil)

;;;###autoload
(defun run-local-var-hooks ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless inhibit-local-var-hooks
    (setq-local inhibit-local-var-hooks t)
    (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                      #'try-run-hook)))

;;;###autoload
(defun run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked. Once HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.

TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            (when (and after-init-time
                       (boundp hook)
                       (symbol-value hook))
              (run-hook-wrapped hook-var #'try-run-hook)
              (set hook-var nil))))
      (let ((target (if (eq hook 'find-file-hook) 'after-find-file hook)))
        (if (functionp target)
            (advice-add target :before fn '((depth . -101)))
          (add-hook target fn (if emacs27-p -101)))))))

;;;###autoload
(defun run-switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or inhibit-switch-buffer-hooks
          (and buffer-or-name
               (eq (current-buffer)
                   (get-buffer buffer-or-name)))
          (and (eq orig-fn #'switch-to-buffer) (car args)))
      (apply orig-fn buffer-or-name args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn buffer-or-name args))
        (with-current-buffer (if (windowp buffer)
                                 (window-buffer buffer)
                               buffer)
          (run-hooks 'switch-buffer-hook))
        buffer))))


;;;; After certin functions have loaded
;;;;

(with-eval-after-load 'projectile
  (defun projectile:get-project-root (&optional dir)
    "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
    (let ((projectile-project-root (unless dir projectile-project-root))
          projectile-require-project-root)
      (projectile-project-root dir))))

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
(defmacro defer-feature! (feature &rest fns)
  "From Doom Emacs.

Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN runs.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if FN is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "my--defer-feature-%s" feature))))
    `(progn
       (delq! ',feature features)
       (advice-add ',fns :before
                   (defun ,advice-fn (&rest _)
                     ;; Some plugins (like yasnippet) will invoke a fn early to parse
                     ;; code, which would prematurely trigger this. In those cases, well
                     ;; behaved plugins will use `delay-mode-hooks', which we can check for:
                     (unless delay-mode-hooks
                       (provide ',feature)
                       ;; ...Otherwise, announce to the world this package has been loaded,
                       ;; so `after!' handlers can react.
                       (dolist (fn ',fn)
                         (advice-remove fn #',advice-fn))))))))

(provide 'lib)
;;; lib.el ends here
