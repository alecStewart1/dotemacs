;;; vc.el --- Version Control in Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: February 20, 2021
;; Modified: February 20, 2021
;;
;; This file is not part of GNU Emacs.
;;
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

;;;###autoload
(defvar magit:fringe-size 14)

;;;###autoload
(defun magit:enlarge-fringe ()
  "Make fringe larger in magit."
  (and (display-graphic-p)
       (derived-mode-p 'magit-mode)
       magit:finge-size
       (let ((left  (or (car-safe magit:fringe-size) magit:fringe-size))
             (right (or (car-safe magit:frinfe-size) magit:fringe-size)))
         (set-window-fringes nil left right))))

;;;###autoload
(defun magit:optimize-process-calls ()
  (when-let (path (executable-find magit-git-executable t))
    (setq-local magit-git-executable path)))

;;;###autoload
(defun magit:reveal-point-if-invisible ()
  (if (derived-mode-p 'org-mode)
      (org-reveal '(4))
    (require 'reveal)
    (reveal-post-command)))

(use-package magit
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
  (transient-default-level . 5)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (defadvice! magit:rever-repo-buffers-deferred (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    (projectile-invalidate-cache nil))

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
  (add-hook 'magit-diff-visit-file-hook #'magit:reveal-point-if-invisible))

;;;; Github Review
;;;;

(use-package github-review
  :after magit)

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

(provide 'vc)
;;; vc.el ends here
