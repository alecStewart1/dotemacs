;;; term.el --- Terminal/Shell packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: February 07, 2021
;; Modified: February 07, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Mainly just for EShell and VTerm.
;;  Don't really see why I need anything else.
;;
;;; Code:

(require 'lib)

;;; Builtin Emacs Packages
;;;

;;;; TODO Eshell
;;;;

(leaf eshell
  :tag "builtin" "eshell" "term"
  :commands (eshell eshell-toggle eshell-frame)
  :defun eshell/alias
  :defvar eshell-prompt-function eshell-command-aliases-list
  :preface
  ;;; Some basics
  ;;;

  (defvar eshell-config-dir (concat my-local-dir "eshell/"))
  (defvar eshell-buffer-name "My Eshell")
  (defvar eshell--buffers (make-ring 25))

  (defun eshell:buffers ()
    "TODO"
    (ring-elements eshell--buffers))

  (defun eshell:unused-buffer (&optional new-p)
    (or (unless new-p
          (loop for buf in (eshell-buffers)
                if (and (buffer-live-p buf)
                        (not (get-buffer-window buf t)))
                return buf))
        (generate-new-buffer eshell-buffer-name)))

  (defun eshell:run-command (command &optional buffer)
    "TODO"
    (let ((buffer (or buffer (if (eq major-mode 'eshell-mode)
                                 (current-buffer)
                               (cl-find-if #'buffer-live-p (eshell-buffers))))))
      (unless buffer
        (user-error "No living eshell buffers available"))
      (unless (buffer-live-p buffer)
        (user-error "Cannot operate on a dead buffer"))
      (with-current-buffer buffer
        (goto-char eshell-last-output-end)
        (goto-char (line-end-position))
        (insert command)
        (eshell-send-input nil t))))

  ;;; Completion
  ;;;

  (defun eshell:pcomplete ()
    "Use pcomplete with completion-in-region backend instead of popup window at
  bottom. This ties pcomplete into ivy or helm, if they are enabled."
    (interactive)
    (require 'pcomplete)
    (if (and (bound-and-true-p company-mode)
             (or company-candidates
                 (and (company-pcomplete-available)
                      (company-pcomplete--prefix)
                      (company-pcomplete--candidates))))
        (call-interactively #'company-pcomplete)
      (ignore-errors (pcomplete-std-complete))))

  ;;; Toggle eshell or create a new frame solely for eshell
  ;;;

  (defun eshell:toggle (arg &optional command)
    "Toggle eshell in a popup window, with COMMAND if specified."
    (interactive "P")
    (let ((eshell-buffer (get-buffer-create
                          (format "Eshell Popup - %s"
                                  (if (bound-and-true-p persp-mode)
                                      (safe-persp-name (get-current-persp))
                                    "Main"))))
          confirm-kill-processes
          current-prefix-arg)
      (when arg
        (when-let (win (get-buffer-window eshell-buffer))
          (delete-window win))
        (when (buffer-live-p eshell-buffer)
          (with-current-buffer eshell-buffer
            (fundamental-mode)
            (erase-buffer))))
      (if-let (win (get-buffer-window eshell-buffer))
          (if (eq (select-window) win)
              (let (confirm-kill-processes)
                (delete-window win)
                (ignore-errors (kill-buffer eshell-buffer)))
            (select-window win)
            (goto-char (point-max)))
        (with-current-buffer (pop-to-buffer eshell-buffer)
          (if (eq major-mode 'eshell-mode)
              (run-hooks 'eshell-mode-hook)
            (eshell-mode))
          (when command
            (eshell:run-command command eshell-buffer))))))

  (defun eshell:frame (&optional command)
    "Open a frame dedicated to eshell, with COMMAND if specified.

Once the eshell process is killed, the previous frame layout is restored."
    (interactive "P")
    (let ((buf (eshell--unused-buffer 'new)))
      (unless (frame-parameter nil 'saved-wconf)
        (set-frame-parameter nil 'saved-wconf (current-window-configuration)))
      (delete-other-windows)
      (with-current-buffer (switch-to-buffer buf)
        (eshell-mode)
        (when command
          (eshell:run-command command buf)))
      buf))

  ;;; View eshell command histroy with Consult
  ;;;

  (defvar consult--eshell-hist nil)

  (defun consult:eshell-hist-action (cmd)
    (let ((buf (if (eq major-mode 'eshell-mode)
                   (current-buffer))))
      (with-current-buffer buf
        (goto-char eshell-last-output-end)
        (goto-char (line-end-position))
        (insert cmd))))

  (defvar consult:eshell-hist-source
    `(:name "Eshell History"
      :narrow ?<
      :face 'eshell-prompt-face
      :histroy 'eshell-history-ring
      :action ,#'consult--eshell-hist-action
      :items
      ,(lambda ()
         ((let ((hist (if (not (ring-empty-p eshell-history-ring))
                          (delete-dups (ring-elements eshell-history-ring))
                        (message "No history"))))
            hist)))))

  (defun consult:eshell-history ()
    (interactive)
    (let ((hist (consult--multi consult:eshell-hist-source
                                :prompt "Command: "
                                :history 'consult--eshell-hist
                                :require-match t
                                :sort nil)))
      (unless (cdr hist)
        (eshell-list-history))))

  (defun eshell:search-history ()
    "Search the eshell command history with consult & selectrum."
    (interactive)
    (require 'em-hist)
    (if (package-installed-p 'consult)
        (consult:eshell-history)
      (eshell-list-history)))

  ;;; Commands
  ;;;
  (defun eshell/less (&rest args)
    "Invoke `view-file' on a file (ARGS).
\"less +42 foo\" will go to line 42 in the buffer for foo."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (eshell-view-file file)
            (forward-line line))
        (eshell-view-file (pop args)))))

  (defalias 'eshell/more #'eshell/less)

  (defun eshell/cd-to-project ()
    "Change to the project root of the current directory."
    (eshell/cd (projectile-project-root (eshell/pwd))))

  (defun eshell/quit-and-close (&rest _)
    "Quit the current eshell buffer and close the window it's in."
    (setq-local +eshell-kill-window-on-exit t)
    (throw 'eshell-terminal t))

  (defun eshell/mkdir-and-cd (dir)
    "Create a directory (DIR) then cd into it."
    (make-directory dir t)
    (eshell/cd dir))

  ;;; Aliases
  ;;;

  (defvar eshell:my-aliases
    '(("q"  "exit")           ; built-in
      ("f"  "find-file $1")
      ("ff" "find-file $1")
      ("d"  "dired $1")
      ("bd" "eshell-up $1")
      ("rg" "rg --color=always $*")
      ("l"  "ls -lh $*")
      ("ll" "ls -lah $*")
      ("la" "ls -aAFh $*")
      (".." "cd ..")
      ("cdp" "cd-to-project")
      ("mkdir" "mkdir -p $*")
      ("rm" "rm -i $*")
      ("cp" "cp -i $*")
      ("mv" "mv -i $*")
      ("git" "git --no-pager $*")
      ("gg" "magit-status")
      ("pass" "gopass $*")
      ("clear" "clear-scrollback")))

  (defvar eshell:default-aliases nil)

  (defun eshell:set-alias! (&rest aliases)
    (or (cl-evenp (length aliases))
        (signal 'wrong-number-of-arguments (list 'even (length aliases))))
    (with-eval-after-load 'em-alias
      (while aliases
        (let ((alias (pop aliases))
              (command (pop aliases)))
          (if-let* ((oldval (assoc alias eshell:my-aliases)))
              (setcdr oldval (list command))
            (push (list alias command) eshell:my-aliases))))
      (when (boundp 'eshell-command-aliases-list)
        (if eshell:default-aliases
            (setq eshell-command-aliases-list
                  (append eshell:default-aliases eshell:my-aliases))
          (setq eshell-command-aliases-list eshell:my-aliases)))))
  :custom
  (eshell-banner-message .
     '(format "%s %s\n"
         (propertize (format " %s " (string-trim (buffer-name)))
                     'face 'mode-line-highlight)
         (propertize (current-time-string)
                     'face 'font-lock-keyword-face)))
  (eshell-scroll-to-bottom-on-input . 'all)
  (eshell-scroll-to-bottom-on-output . 'all)
  (eshell-kill-processes-on-exit . t)
  (eshell-hist-ignoredups . t)
  (eshell-input-filter . (lambda (input) (not (string-match-p "\\`\\s-+" input))))
  (eshell-prompt-regexp . "^.* ~> ")
  (eshell-glob-case-insensitive .  t)
  (eshell-error-if-no-glob . t)
  (eshell-term-name . "xterm-256color")
  :advice
  ;; Need to do this before we add our aliases
  (:override eshell-write-aliases-list ignore)
  :config
  (add-hook 'eshell-mode-hook #'smartparens-mode)
  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  ;;; Alias Stuff
  ;;;

  (eval-after-load 'em-alias
    (setq eshell:default-aliases eshell-command-aliases-list
          eshell-command-aliases-list
          (append eshell-command-aliases-list
                  eshell:my-aliases)))

  ;;; Running CLI commands in Emacs
  ;;;

  (after! em-term
    (pushnew! eshell-visual-commands "tmux" "htop" "vim" "nvim" "ncmpcpp"))

  ;;; Keys
  ;;;

  (define-key 'eshell-mode-map (kbd "C-e") #'end-of-line)
  (define-key 'eshell-mode-map (kbd "C-a") #'beginning-of-line)
  (define-key 'eshell-mode-map (kbd "C-s") #'eshell:search-history))

;;; External Packages
;;;

;;;; Eshell Enhancements
;;;;

(leaf eshell-up
  :ensure t
  :tag "external" "eshell" "complimentary" "term"
  :commands eshell-up eshell-up-peek)

(leaf esh-help
  :ensure t
  :tag "external" "eshell" "complimentary" "term"
  :after eshell
  :config (setup-esh-help-eldoc))

(leaf eshell-did-you-mean
  :ensure t
  :tag "external" "eshell" "complimentary" "term"
  :after esh-mode ; Specifically esh-mode, not eshell
  :config
  (eshell-did-you-mean-setup)
  ;; HACK There is a known issue with `eshell-did-you-mean' where it does not
  ;;      work on first invocation, so we invoke it once manually by setting the
  ;;      last command and then calling the output filter.
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found"))

(leaf eshell-syntax-highlighting
  :ensure t
  :tag "external" "eshell" "complimentary" "term"
  :after esh-mode
  :leaf-defer nil
  :config
  (eshell-syntax-highlighting-global-mode +1))

;;;; VTerm
;;;;

(leaf vterm
  :when (bound-and-true-p module-file-suffix)
  :ensure t
  :tag "external" "term"
  :commands vterm vterm-mode
  :preface (setq vterm-install t)
  :config
  (setq vterm-kill-buffer-on-exit t)
  :defer-config
  (setq confirm-kill-processes nil)
  (setq hscroll-margin 0))

(provide 'term)
;;; term.el ends here
