;;; emacsy.el --- Things that are very Emacs-y -*- lexical-binding: t; -*-
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

(require 'lib)

;;; Builtin Emacs packages
;;;

;;;; Dired
;;;;

(leaf dired
  :tag "builtin" "emacsy"
  :commands dired-jump
  :hook (dired-load-hook . (lambda ()
                            (require 'dired-x)
                            (require 'dired-aux)
                            (require 'dired-guess)))
  :custom
  ; don't prompt to revert; just do it
  (dired-auto-revert-buffer . t)
  ; don't pass --dired to ls
  (dired-use-ls-dired . nil)
  ; suggest a target for moving/copying intelligently
  (dired-dwim-target . t)
  (dired-hide-details-hide-symlink-targets . nil)
  (dired-omit-verbose . nil)
  (dired-omit-files .
   "\\`[.]?#\\|\\`[.][.]?\\'\\|^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\|git\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")
  (dired-garbage-files-regexp
   "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")
  ; Always copy/delete recursively
  (dired-recursive-copies . 'always)
  (dired-recursive-deletes . 'top)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (eval-after-load 'projectile
    (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook))

  (define-key dired-mode-map (kbd "C-c C-e") #'wdired-change-to-wdired-mode))

(leaf image-dired
  :tag "builtin" "emacsy"
  :commands image-dired image-dired-display-thumb image-dired-display-thumbs image-dired-minor-mode
  :custom
  `(; Where to store image caches
    (image-dired-dir . ,(concat my-cache-dir "image-dired/"))
    (image-dired-db-file . ,(concat image-dired-dir "db.el"))
    (image-dired-gallery-dir . ,(concat image-dired-dir "gallery/"))
    (image-dired-temp-image-file . ,(concat image-dired-dir "temp-image"))
    (image-dired-temp-rotate-image-file . ,(concat image-dired-dir "temp-rotate-image"))
    (image-dired-thumb-size . 150)))

;;;; IBuffer
;;;;

(leaf ibuffer
  :tag "builtin" "emacsy"
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("q" . kill-current-buffer))
  :custom
  (ibuffer-show-empty-filter-groups . nil)
  (ibuffer-filter-group-name-face . '(:inherit (success bold)))
  (ibuffer-formats .
   `((mark modified read-only locked
           ,@(if (featurep! +icons)
                 `(;; Here you may adjust by replacing :right with :center
                   ;; or :left According to taste, if you want the icon
                   ;; further from the name
                   " " (icon 2 2 :left :elide)
                   ,(propertize " " 'display `(space :align-to 8)))
               '(" "))
           (name 18 18 :left :elide)
           " " (size 9 -1 :right)
           " " (mode 16 16 :left :elide)
           ,@(when (require 'ibuffer-vc nil t)
               '(" " (vc-status 12 :left)))
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename)))
  :config
  (define-ibuffer-column icon (:name "  ")
    (let ((icon (if (and (buffer-file-name)
                         (all-the-icons-auto-mode-match?))
                    (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  (define-ibuffer-column size
    (:name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;;;; Electric
;;;;

(leaf electric
  :tag "builtin" "emacsy"
  :hook (first-file-hook . electric-quote-mode)
  :preface
  (defvar-local electric--indent-words '()
    "The list of electric words. Typing these will trigger reindentation of the
current line.")
  :config
  (setq-default electric-indent-chars '(?\n ?\^?))

  (add-hook! 'electric-indent-functions
    (defun electric-indent-char-fn (_c)
      (when (and (eolp) electric--indent-words)
        (save-excursion
          (backward-word)
          (looking-at-p (concat "\\<" (regexp-opt electric--indent-words))))))))

;;; External packages
;;;

;;;; Undo-fu
;;;;

(leaf undo-fu
  :ensure t
  :doc "A more sane undo."
  :tag "external" "emacsy"
  :leaf-defer nil
  :hook (first-buffer-hook . undo-fu-mode)
  :setq
  (undo-limit . 400000)
  (undo-strong-limit . 3000000)
  (undo-outer-limit . 3000000)
  :config
  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))

(leaf undo-fu-session
  :ensure t
  :leaf-defer nil
  :hook (undo-fu-mode-hook . global-undo-fu-session-mode)
  :preface
  (setq undo-fu-session-directory (concat my-cache-dir "undo-fu-session/")
        undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :defer-config
  (when (executable-find "zstd")
    (advice-add #'undo-fu-session--make-file-name :filter-return
                (defun undo-fu-session-use-zstd (filename)
                  (if undo-fun-session-compression
                      (concat (file-name-sans-extension filename) ".zst")
                    filname)))))

;;;; Group projects in IBuffer
;;;;

(leaf ibuffer-projectile
  :ensure t      
  ;; Group ibuffer's list by project root
  :hook (ibuffer-hook . ibuffer-projectile-set-filter-groups)
  :custom
  `((ibuffer-projectile-prefix .
     ,(if (package-installed-p 'all-the-icons)
         ,(concat (all-the-icons-octicon
                  "file-directory"
                  :face ibuffer-filter-group-name-face
                  :v-adjust -0.05)
                 " ")
       "Project: "))))

;;;; Bufler
;;;;

;; (use-package bufler
;;   :straight (bufler :type git :host github :repo "alphapapa/bufler.el"
;;                     :files (:defaults (:exclude "helm-bufler.el")))
;;   :hook (post-command . bufler-mode)
;;   :config
;;   (bufler-defgroups
;;    (group
;;     (auto-workspace))
;;    (group
;;     (auto-projectile))
;;    (group
;;     (auto-project))
;;    (dir user-emacs-directory)
;;    (group
;;     (dir org-directory)
;;     (group
;;      (auto-indirect)
;;      (auto-file))
;;     (group-not "*Special*" (auto-file))
;;     (auto-mode))
;;    (auto-mode)
;;    (auto-directory)
;;    (group
;;     ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
;;     (group-or "*Help/Info*"
;;               (mode-match "*Help*" (rx bos "help-"))
;;               (mode-match "*Info*" (rx bos "info-"))))
;;    (group
;;     (group-and "*Special*"
;;                (lambda (buffer)
;;                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
;;                                       buffer)
;;                              (funcall (mode-match "Dired" (rx bos "dired"))
;;                                       buffer)
;;                              (funcall (auto-file) buffer))
;;                    "*Special*")))
;;     (group
;;      (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
;;      (auto-directory))
;;     (group
;;      (name-match "**Special**"
;;                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
;;     (auto-mode))))

(provide 'emacsy)
;;; emacsy.el ends here
