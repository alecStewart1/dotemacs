;;; app.el --- Using Emacs Like an actual application -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: June 28, 2021
;; Modified: June 28, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'mode-local)
(require 'lib)

;;;; Builtin Packages


;;;;; Allow us to enter passwords in Emacs
;;;;;

(use-package epg-config
  :demand t
  :init
  (setq epg-pinentry-mode 'loopback))

;;;; External Packages
;;;;

;;;;; Password Entry in Emacs
;;;;;

(use-package pinentry
  :hook
  (after-init . pinentry-start))

;;;;; Emacs Multimedia System
;;;;;

(use-package emms
  :defer t
  :custom
  (emms-directory  (concat my-etc-dir "emms"))
  (emms-cache-file (concat my-cache-dir "emms"))
  :config
  (emms-all)
  (emms-default-players))

;;;;; RSS Feeds in Emacs
;;;;;

;;;###autoload
(defun elfeed:wrap ()
  "Enhances an elfeed entry's readability by wrapping it to a width of `fill-column'."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    (setq-local shr-use-fonts nil)
    (setq-local shr-width 85)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun elfeed:cleanup ()
  "Clean up after an elfeed session. Kill all elfeed and elfeed-org files."
  (interactive)
  ;; `delete-file-projectile-remove-from-cache' slows down `elfeed-db-compact'
  ;; tremendously, so we disable the projectile cache:
  (let (projectile-enable-caching)
    (elfeed-db-compact))
  (let ((buf (previous-buffer)))
    (when (null buf)
      (switch-to-buffer (fallback-buffer))))
  (let ((search-buffers (buffers-in-mode 'elfeed-search-mode))
        (show-buffers (buffers-in-mode 'elfeed-show-mode))
        kill-buffer-query-functions)
    (dolist (file (bound-and-true-p rmh-elfeed-org-files))
      (when-let (buf (get-file-buffer (expand-file-name file org-directory)))
        (kill-buffer buf)))
    (dolist (b search-buffers)
      (with-current-buffer b
        (remove-hook 'kill-buffer-hook #'elfeed:cleanup :local)
        (kill-buffer b)))
    (mapc #'kill-buffer show-buffers)))

;;;###autoload
(defun elfeed:put-sliced-image (spec alt &optional flags)
  "TODO"
  (letf! (defun insert-image (image &optional alt _area _slice)
           (let ((height (cdr (image-size image t))))
             (insert-sliced-image image alt nil (max 1 (/ height 20.0)) 1)))
    (shr-put-image spec alt flags)))

;;;###autoload
(defun elfeed:render-image-tag-without-underline (dom &optional url)
  "TODO"
  (let ((start (point)))
    (shr-tag-img dom url)
    ;; And remove underlines in case images are links, otherwise we get an
    ;; underline beneath every slice.
    (put-text-property start (point) 'face '(:underline nil))))

(use-package elfeed
  :commands elfeed
  :custom
  (elfeed-db-directory (concat my-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat my-local-dir "elfeed/enclosures/"))
  (elfeed-search-filter "@2-week-ago ")
  (elfeed-show-entry-switch #'pop-to-buffer)
  (shr-max-image-proportion 0.8)
  :config
  (make-directory elfeed-db-directory t)

  (add-hook 'elfeed-show-mode-hook #'elfeed:wrap)
  (add-hook! 'elfeed-search-mode-hook
    (add-hook 'kill-buffer-hook #'elfeed:cleanup nil 'local))

  (setq-mode-local elfeed-show-mode
                   shr-put-image-function #'elfeed:put-sliced-image
                   shr-external-rendering-functions '((img . elfeed:render-image-tag-without-underline))))

(use-package elfeed-org
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (defadvice! elfeed-org:skip-missing-files (&rest _)
    "TODO"
    :before '(elfeed rmh-elfeed-org-mark-feed-ignore elfeed-or-export-opml)
    (unless (file-name-absolute-p (car rmh-elfeed-org-files))
      (let* ((default-directory org-directory)
             (files (mapcar #'expand-file-name rmh-elfeed-org-files)))
        (dolist (file (cl-remove-if #'file-exists-p files))
          (message "elfeed-org: ignoring %S because it can't be read" file))
        (setq rmh-elfeed-org-files (cl-remove-if-not #'file-exists-p files)))))

  (elfeed-org))

(provide 'app)
;;; app.el ends here
