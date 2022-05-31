;;; termy.el --- Terminal/Shell packages -*- lexical-binding: t; -*-
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
;;  Mainly just for EShell and VTerm.
;;  Don't really see why I need anything else.
;;
;;; Code:

(require 'lib)
(require 'cl-lib)
(require 'mode-local)

;;; Builtin Emacs Packages
;;;

;;;; Eshell
;;;;

;;;###autoload
(defvar eshell-config-dir (concat my-local-dir "eshell/"))

;;;###autoload
(defvar eshell-buffer-name "My Eshell")

;;;###autoload
(defvar eshell--buffers (make-ring 25))

;;;###autoload
(defun eshell:buffers ()
  "TODO"
  (ring-elements eshell--buffers))

(defun eshell:unused-buffer (&optional new-p)
  (or (unless new-p
        (cl-loop for buf in (eshell-buffers)
              if (and (buffer-live-p buf)
                      (not (get-buffer-window buf t)))
              return buf))
      (generate-new-buffer eshell-buffer-name)))

;;;###autoload
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

;;; TODO Completion
;;;


;;; Toggle eshell or create a new frame solely for eshell
;;;

;;;###autoload
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

;;;###autoload
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

;;; Completion for EShell with Corfu + Cape
;;;

;; Make pcomplete behave with Corfu and Cape
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;;;###autoload
(defvar cape:eshell-capf (list
                          (cape-capf-buster
                           (cape-super-capf
                            #'pcomplete-completions-at-point
                            #'cape-abbrev))
                          #'cape-file))

(defun eshell:setup-completion ()
  "Setup completion for EShell with ‘corfu’ and ‘cape’."
  (setq-mode-local eshell-mode
                   corfu-auto nil
                   corfu-quit-at-boundary t
                   corfu-quit-no-match t
                   completion-at-point-functions cape:eshell-capf))


;;; View eshell command histroy with Consult
;;;

;;;###autoload
(defvar consult--eshell-hist nil)

;;;###autoload
(defun consult:eshell-hist-action (cmd)
  (let ((buf (if (eq major-mode 'eshell-mode)
                 (current-buffer))))
    (with-current-buffer buf
      (goto-char eshell-last-output-end)
      (goto-char (line-end-position))
      (insert cmd))))

;;;###autoload
(defvar consult:eshell-hist-source
  `(:name "Eshell History"
    :narrow ?<
    :face 'eshell-prompt-face
    :histroy 'eshell-history-ring
    :action ,#'consult--eshell-hist-action
    :items
    ,(lambda ()
       (let ((hist (if (not (ring-empty-p eshell-history-ring))
                        (delete-dups (ring-elements eshell-history-ring))
                      (message "No history"))))
          hist))))

;;;###autoload
(defun consult:eshell-history ()
  (interactive)
  (let ((hist (consult--read consult:eshell-hist-source
                             :prompt "Command: "
                             :history 'consult--eshell-hist
                             :require-match t
                             :sort nil)))
    (unless (cdr hist)
      (eshell-list-history))))

;;;###autoload
(defun eshell:search-history ()
  "Search the eshell command history with consult & selectrum."
  (interactive)
  (require 'em-hist)
  (if (package-installed-p 'consult)
      (consult:eshell-history)
    (eshell-list-history)))


;;; Commands
;;;

;;;###autoload
(defvar eshell:kill-window-on-exit nil
  "If non-nil, eshell will close windows along with its eshell buffers.")

;;;###autoload
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

;;;###autoload
(defun eshell/cd-to-project ()
  "Change to the project root of the current directory."
  (eshell/cd (projectile:get-project-root (eshell/pwd))))

;;;###autoload
(defun eshell/quit-and-close (&rest _)
  "Quit the current eshell buffer and close the window it's in."
  (setq-local eshell:kill-window-on-exit t)
  (throw 'eshell-terminal t))

;;;###autoload
(defun eshell/mkdir-and-cd (dir)
  "Create a directory (DIR) then cd into it."
  (make-directory dir t)
  (eshell/cd dir))


(use-package eshell
  :ensure nil
  :commands (eshell eshell-toggle eshell-frame)
  :defines eshell-prompt-function
  :functions eshell/alias
  :custom
  (eshell-banner-message
     '(format "%s %s\n"
         (propertize (format " %s " (string-trim (buffer-name)))
                     'face 'mode-line-highlight)
         (propertize (current-time-string)
                     'face 'font-lock-keyword-face)))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input))))
  (eshell-prompt-regexp "^.* ~> ")
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  (eshell-term-name "xterm-256color")
  :config
  (add-hook! 'eshell-mode-hook
    (lambda ()
      (set-window-fringes nil 0 0)
      (set-window-margins nil 1 nil)
      (visual-line-mode +1)
      (set-display-table-slot standard-display-table 0 ?\))))

  (setq-local corfu-auto nil
              corfu-quit-at-boundary t
              corfu-quit-no-match t)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  (advice-add #'eshell-write-aliases-list :override #'ignore)

  (with-eval-after-load 'esh-mode
    (define-key eshell-mode-map (kbd "C-j") #'eshell-next-matching-input-from-input)
    (define-key eshell-mode-map (kbd "C-k") #'eshell-previous-matching-input-from-input)
    (define-key eshell-mode-map (kbd "C-e") #'end-of-line)
    (define-key eshell-mode-map (kbd "C-s") #'eshell:search-history)
    (define-key eshell-mode-map (kbd "C-l") (lambda () (interactive)
                                              (eshell/clear-scrollback)
                                              (eshell-emit-prompt)))))

;;;; Esh-Modules
;;;;

(use-package esh-module
  :ensure nil
  :defer t
  :custom
  (eshell-module-list '(eshell-tramp)))

;;;; Smarter EShell
;;;;

(use-package em-smart
  :ensure nil
  :defer t
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  :config
  (eshell-smart-initialize))

;;;; Alias stuff
;;;;

(use-package em-alias
  :ensure nil
  :defer t
  :preface
  (defvar eshell:my-aliases
    '(("q"  "quit-and-close")
      ("f"  "find-file $1")
      ("ff" "find-file-other-window $1")
      ("d"  "dired $1")
      ("bd" "eshell-up $1")
      ("rg" "rg --color=always $*")
      ("l"  "ls -lh $*")
      ("ll" "ls -lah $*")
      ("la" "ls -laAFh $*")
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
  :config
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  (defalias 'eshell/more #'eshell/less)
  
  (setq eshell-command-aliases-list
        (append eshell-command-aliases-list
                eshell:my-aliases)))

(use-package em-xtra
  :ensure nil
  :defer t)

;;; External Packages
;;;

;;;; Eshell Enhancements
;;;;

(use-package eshell-up
  :commands eshell-up eshell-up-peek)

(use-package esh-help
  :disabled t
  :after eshell
  :config (setup-esh-help-eldoc))

(use-package eshell-did-you-mean
  :after esh-mode ; Specifically esh-mode, not eshell
  :config
  (eshell-did-you-mean-setup)
  ;; HACK There is a known issue with `eshell-did-you-mean' where it does not
  ;;      work on first invocation, so we invoke it once manually by setting the
  ;;      last command and then calling the output filter.
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found"))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;;;; VTerm
;;;;
;;;; Colors are a little jank

(use-package vterm
  :when (bound-and-true-p module-file-suffix)
  :commands vterm vterm-mode
  :hook (vterm-mode . hide-mode-line-mode)
  :preface
  (setq vterm-install t)
  :init
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000)
  (setq confirm-kill-processes nil)
  (setq hscroll-margin 0))

(provide 'termy)
;;; termy.el ends here
