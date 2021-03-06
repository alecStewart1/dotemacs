
;;; version-control.el --- Version Control in Emacs -*- lexical-binding: t; -*-
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

(require 'lib)

;;; VC

(remove-hook 'find-file-hooks 'vc-find-file-hook)

;;;; Git
;;;;

;;; This can slow things down, so I just comment it out for now.
;;
;; (use-package git-gutter
;;   :commands git-gutter:revert-hunk git-gutter:stage-hunk
;;   :custom
;;   (git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
;;   (git-gutter:handled-backedns (cons 'git
;;                                      (cl-remove-if-not
;;                                       #'executable-find
;;                                       (list 'hg 'svn 'bzr)
;;                                       :key #'symbol-name)))
;;   :preface
;;   (defun vc-gutter-init-maybe ()
;;     "Enable `git-gutter-mode' in the current buffer.

;; If the buffer doesn't represent an existing file, `git-gutter-mode's activation
;; is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
;;     (let ((file-name (buffer-file-name (buffer-base-buffer))))
;;       (when (or +vc-gutter-in-remote-files
;;                 (not (file-remote-p (or file-name default-directory))))
;;         (if (null file-name)
;;             (add-hook 'after-save-hook #'vc-gutter-init-maybe nil 'local)
;;           (when (and (vc-backend file-name)
;;                      (progn
;;                        (require 'git-gutter)
;;                        (not (memq major-mode git-gutter:disabled-modes))))
;;             (if (and (display-graphic-p)
;;                      (require 'git-gutter-fringe nil t))
;;                 (setq-local git-gutter:init-function      #'git-gutter-fr:init
;;                             git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
;;                             git-gutter:clear-function     #'git-gutter-fr:clear
;;                             git-gutter:window-width -1)
;;               (setq-local git-gutter:init-function      'nil
;;                           git-gutter:view-diff-function #'git-gutter:view-diff-infos
;;                           git-gutter:clear-function     #'git-gutter:clear-diff-infos
;;                           git-gutter:window-width 1))
;;             (git-gutter-mode +1)
;;             (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local))))))

;;   (defun vc-gutter-update (&rest _)
;;     (when (and git-gutter-mode
;;                (not (memq this-command '(git-gutter:stage-hunk
;;                                          git-gutter:revert-hunk)))
;;                (not inhibit-redisplay))
;;       (ignore (git-gutter))))

;;   (defun vc-gutter--fix-linearity-of-hunks (diffinfos is-reverse)
;;     (cl-position-if (let ((lineo (line-number-at-pos)))
;;                       (lambda (line)
;;                         (funcall (if is-reverse #'> #'< ) lineo line)))
;;                     diffinfos
;;                     :key #'git-gutter-hunk-start-line
;;                     :from-end is-reverse))
;;   :config
;;   (add-hook! 'find-file-hook #'vc-gutter-init-maybe)

;;   (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

;;   (advice-add #'magit-stage-file :after #'vc-gutter-update)
;;   (advice-add #'magit-unstage-file :after #'vc-gutter-update)

;;   (advice-add #'git-gutter:search-near-diff-index :override #'vc-gutter--fix-linearity-of-hunks))

;;;; Magit
;;;;

(defvar magit:stale-p nil)

(defun magit:revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable 'magit:stale-p)
    (when buffer-file-name
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t t)))))

;;;###autoload
(defun magit:mark-stale-buffers ()
  "Revert all visible buffers and mark buried buffers as stale.
Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer)
          (magit:revert-buffer buffer)
        (with-current-buffer buffer
          (setq-local magit:stale-p t))))))

;;;###autoload
(defun magit:revert-buffer-maybe ()
  "Update `vc' and `git-gutter' if out of date."
  (when magit:stale-p
    (magit:revert-buffer (current-buffer))))

;;;###autoload
(defvar magit:fringe-size 14)

;;;###autoload
(defun magit:enlarge-fringe ()
  "Make fringe larger in magit."
  (and (display-graphic-p)
       (derived-mode-p 'magit-mode)
       magit:fringe-size
       (let ((left  (or (car-safe magit:fringe-size) magit:fringe-size))
             (right (or (car-safe magit:fringe-size) magit:fringe-size)))
         (set-window-fringes nil left right))))

;;;###autoload
(defun magit:optimize-process-calls ()
  "TODO"
  (when-let (path (executable-find magit-git-executable t))
    (setq-local magit-git-executable path)))

;;;###autoload
(defun magit:reveal-point-if-invisible ()
  "TODO"
  (if (derived-mode-p 'org-mode)
      (org-reveal '(4))
    (require 'reveal)
    (reveal-post-command)))

;;; Taken from:
;;; https://emacs.stackexchange.com/questions/20154/how-can-i-stage-all-changes-and-commit-them-without-displaying-commit-message-bu
;;;###autoload
(defun magit:stage-all-and-commit (msg)
  "Stage all modified files with commit message MSG."
  (interactive "sCommit Message: ")
  (magit-stage-modified)
  (magit-commit (list "-m" msg)))

(use-package magit
  :defer t
  :commands magit-file-delete
  :custom
  (magit-auto-revert-mode nil)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-bury-buffer-function #'magit-mode-quit-window)
  (transient-levels-file (concat my-etc-dir "transient/levels"))
  (transient-values-file (concat my-etc-dir "transient/values"))
  (transient-history-file (concat my-etc-dir "transient/history"))
  (transient-default-level 5)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (defadvice! magit:revert-repo-buffers-deferred (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    (magit:mark-stale-buffers))

  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (define-key transient-map [escape] #'transient-quit-one)

  (add-hook! 'magit-mode-hook
    (add-hook! 'window-configuration-change-hook :local
        #'magit:enlarge-fringe))

  (add-hook 'magit-status-mode-hook #'magit:optimize-process-calls)
  (add-hook 'magit-diff-visit-file-hook #'magit:reveal-point-if-invisible)

  (define-key magit-status-mode-map (kbd "q") #'magit-mode-quit-window))

;;;; Github Review
;;;;

(use-package code-review
  :after magit
  ;; :config
  ;; (transient-append-suffix 'magit-merge "i"
  ;;   '("y" "Review pull request")
  ;;   )
  )

;;;; TODOs for Magit
;;;;

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))

;;;; Gitflow for Magit
;;;;

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(provide 'version-control)
;;; version-control.el ends here
