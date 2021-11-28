;;; emacsy.el --- Some things that are very Emacs-y -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec
;;
;; Created: January 15, 2021
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

(use-package dired
  :ensure nil
  :commands dired-jump
  :hook (dired-load . (lambda ()
                            (require 'dired-x)
                            (require 'dired-aux)))
  :init
  (setq image-dired-dir                    (concat my-cache-dir "image-dired/")
        image-dired-db-file                (concat image-dired-dir "db.el")
        image-dired-gallery-dir            (concat image-dired-dir "gallery/")
        image-dired-temp-image-file        (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size             150)
  :custom
  ; don't prompt to revert; just do it
  (dired-auto-revert-buffer t)
  ; don't pass --dired to ls
  (dired-use-ls-dired nil)
  ; suggest a target for moving/copying intelligently
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-omit-verbose nil)
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\|git\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")
  (dired-garbage-files-regexp "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")
  ; Always copy/delete recursively
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (eval-after-load 'projectile
    (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook))

  (define-key dired-mode-map (kbd "C-c C-e") #'wdired-change-to-wdired-mode))

(use-package image-dired
  :ensure nil
  :commands (image-dired image-dired-display-thumb image-dired-display-thumbs image-dired-minor-mode))

;;;; IBuffer
;;;;

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-h" . ibuffer-hydra/pretty-body)
         ("q" . kill-current-buffer))
  :pretty-hydra
  ((:title "IBuffer" :color amaranth :quit-key "q")
   ("Navigation"
    (("j" ibuffer-forward-line   "forward")
     ("RET" ibuffer-visit-buffer "visit" :color blue)
     ("k" ibuffer-backward-line  "backward"))
    "Mark"
    (("m" ibuffer-mark-forward   "mark")
     ("u" ibuffer-unmark-forward "unmark")
     ("*" embark:ibuffer-mark    "embark: mark" :color blue))
    "Actions"
    (("D" ibuffer-do-delete     "delete")
     ("S" ibuffer-do-save       "save")
     ("a" embark:ibuffer-action "embark: action"))
    "Other"
    (("g" ibuffer-update        "update")
     ("s" embark:ibuffer-sort   "embark: sort"   :color blue)
     ("/" embark:ibuffer-filter "embark: filter" :color blue))
    "Misc."
    (("o" ibuffer-visit-buffer-other-window "other window"))))
  :custom-face
  (ibuffer-filter-group-name-face '(:inherit (success bold)))
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats
   `((mark modified read-only locked
           ,@(if (package-installed-p 'all-the-icons)
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
    (file-size-human-readable (buffer-size)))

  ;; Embark Keymaps
  (with-eval-after-load 'embark
    (embark-define-keymap embark:ibuffer-mark-map
                          "Marking buffers in IBuffer."
                          ("*" ibuffer-unmark-all)
                          ("M" ibuffer-mark-by-mode)
                          ("m" ibuffer-mark-modified-buffers)
                          ("u" ibuffer-mark-unsaved-buffers)
                          ("s" ibuffer-mark-special-buffers)
                          ("r" ibuffer-mark-read-only-buffers)
                          ("d" ibuffer-mark-dired-buffers)
                          ("b" ibuffer-hydra/pretty-body))

    (embark-define-keymap embark:ibuffer-action-map
                          "Doing actions in IBuffer."
                          ("A" ibuffer-do-view)
                          ("H" ibuffer-do-view-other-frame)
                          ("E" ibuffer-do-eval)
                          ("W" ibuffer-do-view-and-eval)
                          ("F" ibuffer-do-shell-command-file)
                          ("X" ibuffer-do-shell-command-pipe)
                          ("N" ibuffer-do-shell-command-pipe-replace)
                          ("Q" ibuffer-do-query-replace-regexp)
                          ("U" ibuffer-do-replace-regexp)
                          ("V" ibuffer-do-revert)
                          ("b" ibuffer-hydra/pretty-body))

    (embark-define-keymap embark:ibuffer-sort-map
                          "Sorting buffers in IBuffer."
                          ("i" ibuffer-invert-sorting)
                          ("a" ibuffer-do-sort-by-alphabetic)
                          ("v" ibuffer-do-sort-by-recency)
                          ("s" ibuffer-do-sort-by-size)
                          ("f" ibuffer-do-sort-by-filename/process)
                          ("m" ibuffer-do-sort-by-major-mode)
                          ("b" ibuffer-hydra/pretty-body))

    (embark-define-keymap embark:ibuffer-filter-map
                          "Filtering buffers in IBuffer."
                          ("m" ibuffer-filter-by-used-mode)
                          ("M" ibuffer-filter-by-derived-mode)
                          ("n" ibuffer-filter-by-name)
                          ("c" ibuffer-filter-by-content)
                          ("e" ibuffer-filter-by-predicate)
                          ("f" ibuffer-filter-by-filename)
                          (">" ibuffer-filter-by-size-gt)
                          ("<" ibuffer-filter-by-size-lt)
                          ("/" ibuffer-filter-disable)
                          ("b" ibuffer-hydra/pretty-body))))

;;;; Electric
;;;;

(use-package electric
  :ensure nil
  :hook ((find-file . electric-quote-mode)
         (dired-initial-position . electric-quote-mode))
  :preface
  (defvar-local electric:indent-words '()
    "The list of electric words. Typing these will trigger reindentation of the
current line.")
  :config
  (setq-default electric-indent-chars '(?\n ?\^?))

  (add-hook! 'electric-indent-functions
    (defun electric-indent-char-fn (_c)
      (when (and (eolp) electric:indent-words)
        (save-excursion
          (backward-word)
          (looking-at-p (concat "\\<" (regexp-opt electric:indent-words))))))))

;;; External packages
;;;

;;;; Undo-fu
;;;;

(use-package undo-fu
  :hook ((find-file . undo-fu-mode))
  :config
  (setq undo-limit        400000
        undo-strong-limit 3000000
        undo-outer-limit  3000000)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo]    #'undo-fu-only-undo)
              (define-key map [remap redo]    #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))

(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :init
  (setq undo-fu-session-directory (concat my-cache-dir "undo-fu-session/"))
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    (defadvice! undo-fu-session:use-zstd (filename)
      "Have `undo-fu-session--make-file-name' use zstd's .zst file extension for compression on FILENAME."
      :filter-return #'undo-fu-session--make-file-name
      (if undo-fu-session-compression
          (concat (file-name-sans-extension filename) ".zst")
        filname))))

;;;; Group projects in IBuffer
;;;;

(use-package ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer-mode . ibuffer-projectile-set-filter-groups)
  :custom
  (ibuffer-projectile-prefix
     (if (package-installed-p 'all-the-icons)
         (concat (all-the-icons-octicon
                  "file-directory"
                  :face ibuffer-filter-group-name-face
                  :v-adjust -0.05)
                 " ")
       "Project: ")))

;;;; Burly
;;;; TODO maybe use eyebrowse as well along with this?

(use-package burly
  :defer t)

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
