;;; writing.el --- Writing and taking notes in Emacs -*- lexical-binding: t; -*-
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

(require 'mode-local)
(require 'lib)

;;;; Spellin' Good
;;;;

(use-package ispell
  :config
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

;;;; Org-Mode
;;;;

(use-package org
  :preface
  (defvar org-directory nil)
  (defvar org-attach-id-dir nil)

  (setq org-publish-timestamp-directory (concat my-cache-dir "org-timestamps/")
        org-preview-latex-image-directory (concat my-cache-dir "org-latex/"))

  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ol-bibtex
      ol-docview
      org-ctags
      org-habit
      org-tempo
      ol-eshell
      org-annotate-file
      ol-bookmark
      org-checklist
      org-collector
      org-depend
      org-eval
      org-learn
      ol-man
      org-panel))

  (defun org:setup-dirs ()
    (setq org-directory (expand-file-name "~/Documents/Org/"))
    (setq org-id-locations-file (expand-file-name ".orgids" org-directory)))

  (defun org:setup-agenda ()
    (setq org-agenda-files (list org-directory))
    (setq-default
     org-agenda-window-setup 'current-window
     org-agenda-skip-unavailable-files t
     org-agenda-span 10
     org-agenda-start-on-weekday nil
     org-agenda-start-day "-3d"
     org-agenda-files '("~/Documents/Org/agenda.org"
                        "~/Documents/Org/Notes/personal-fitness.org")))

  (defun org:setup-appearance ()
    (setq org-indirect-buffer-display 'current-window
          org-eldoc-breadcrumb-separator " -> "
          org-enforce-todo-dependencies t
          org-entities-user
          '(("flat" "\\flat" nil "" "" "266D" "")
            ("sharp" "\\sharp" nil "" "" "266F" ""))
          org-fontify-quote-and-verse-blocks t
          org-fontify-whole-heading-line t
          org-hide-emphasis-markers t
          org-footnote-auto-label t
          org-hide-leading-stars t
          org-image-actual-width nil
          org-priority-faces
          '((?A . error)
            (?B . warning)
            (?C . success))
          org-startup-indented t
          org-tags-column 0
          org-use-sub-superscripts '{}
          org-log-into-drawer t
          org-log-done 'time)

    (setq org-refile-targets
          '((nil :maxlevel 3)
            (org-agenda-files :maxlevel 3))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)

    (plist-put org-format-latex-options :scale 1.5)

    (with-no-warnings
      (custom-declare-face 'org--todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
      (custom-declare-face 'org--todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
      (custom-declare-face 'org--todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))

    (setq org-todo-keywords
          '((sequence
             "TODO(t)"
             "PROJ(p)"
             "STRT(s)"
             "WAIT(w)"
             "HOLD(h)"
             "MAYBE(m)"
             "CHECK(c)"
             "TO-READ(r)"
             "|"
             "DONE(d)"
             "KILL(k)"
             "READ(R)")
            (sequence
             "[ ](T)"
             "[-](S)"
             "[?](W)"
             "|"
             "[X](D)"))
          org-todo-keyword-faces
          '(("[-]"     . org--todo-active)
            ("STRT"    . org--todo-active)
            ("[?]"     . org--todo-onhold)
            ("WAIT"    . org--todo-onhold)
            ("HOLD"    . org--todo-onhold)
            ("MAYBE"   . org--todo-onhold)
            ("CHECK"   . org--todo-onhold)
            ("TO-READ" . org-todo)
            ("READ"    . org-done)
            ("PROJ"    . org--todo-project)))

    (defadvice! org-eldoc:display-lin (&rest _)
      "TODO"
      :before-until #'org-eldoc-documentation-function
      (when-let (link (org-element-property :raw-link (org-element-context)))
        (format "Link: %s" link)))

  (defun org:setup-babel ()
    (setq org-src-preserve-indentation t
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-confirm-babel-evaluate nil
          org-link-elisp-confirm-function nil
          org-src-window-setup 'other-window)

    (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit))

  (defun org:setup-capture ()
    (with-eval-after-load 'org-capture
      (org-capture-put :kill-buffer t))

    (add-hook 'org-after-refile-insert-hook #'save-buffer)

    (defadvice! org-capture:expand-var-file (file)
      "TODO"
      :filter-args #'org-capture-expand-file
      (if (and (symbolp file) (boundp file))
          (expand-file-name (symbol-value file) org-directory)
        file))

    (when (package-installed-p evil)
      (add-hook 'org-capture-mode-hook #'evil-insert-state))

    (setq org-capture-templates (append org-capture-templates
                                        '(("f" "Fitness Note" entry
                                           (file+olp+datetree "~/Documents/Org/Notes/personal-fitness.org")
                                           "* Log :DAILY: \n %? \n"
                                           :empty-lines 1
                                           :tree-type 'week)
                                          ("i" "Ideas/Thoughts")
                                          ("ii" "Ideas" entry
                                           (file "~/Documents/Org/Notes/ideas.org")
                                           "* %^{Idea Name: } \n %? \n"
                                           :empty-lines 1)
                                          ("it" "Thought" entry
                                           (file "~/Documents/Org/Notes/thoughts.org")
                                           "* %^{Thought: } \n %? \n"
                                           :empty-lines 1))))

    (use-package org-chef
      :after org-capture
      :config
      (setq org-capture-templates (append org-capture-templates
                                          '(("c" "Cookbook" entry (file "~/Documents/org/cookbook.org")
                                             "%(org-chef-get-recipe-from-url)"
                                             :empty-lines 1)
                                            ("m" "Manual Cookbook" entry (file "~/Documents/Org/cookbook.org")
                                             "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n"))))))

  (defun org:setup-attachments ()
    (setq org-attach-store-link-p t
          org-attach-use-inheritance t)

    (use-package org-attach
      :commands (org-attach-new
                 org-attach-open
                 org-attach-open-in-emacs
                 org-attach-reveal-in-emacs
                 org-attach-url
                 org-attach-set-directory
                 org-attach-sync)
      :config
      (unless org-attach-id-dir
        (setq org-attach-id-dir (expand-file-name ".attach/" org-directory)))
      (with-eval-after-load 'projectile
        (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir))))

  (defun org:setup-custom-links ()
    (org-link-set-parameters
     "file"
     :face (lambda (path)
             (if (or (file-remote-p path)
                     (file-exists-p path))
                 'org-link
               'error)))
    (pushnew! org-link-abbrev-alist
              '("github"      . "https://github.com/%s")
              '("youtube"     . "https://youtube.com/watch?v=%s")
              '("google"      . "https://google.com/search?q=")
              '("gimages"     . "https://google.com/images?q=%s")
              '("gmap"        . "https://maps.google.com/maps?q=%s")
              '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
              '("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
              '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
              '("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s"))

    (use-package org-yt
      :init
      (advice-add org-yt-image-data-fun :before-while (lambda (&rest _)
                                                        (not (eq org-display-remote-inline-images 'skip))))))

  (defun org:setup-export ()
    (setq org-export-with-smart-quotes t
          org-html-validation-link nil)
    (add-to-list 'org-export-backends '(md odt))

    (defadvice! org-export:dont-trigger-save-hooks-on-export (orig-fn &rest args)
      "TODO"
      :around #'org-export-to-file
      (let (before-save-hook after-save-hook)
        (apply orig-fn args)))

    (use-package ox-hugo
      :after ox))

  (defun org:setup-habit ()
    (add-hook! 'org-agenda-mode-hook
      (defun org-habit:resize-graph ()
        "TODO"
        ())))

  (defun org:setup-hacks ()
    (setf (alist-get 'file org-link-frame-setup) #'find-file)
    (add-to-list 'org-file-apps '(directory . emacs))
    (with-eval-after-load 'org-eldoc
      (puthash "org" #'ignore org-eldoc-local-functions-cache)
      (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

    (defadvice! org:export-agenda-from-recentf (orig-fn file)
      "TODO"
      :around #'org-get-agenda-file-buffer
      (let ((recentf-exclude (list (lambda (_file) t))))
        (funcall orig-fn file)))

    (defadvice! org:fix-inline-images-imagemagick (orig-fn &rest args)
      "TODO"
      :around #'org-display-inline-images
      (letf! (defun create-image (file-or-data &optional type data-p &rest props)
               (let ((type (if (plist-get props :width) type)))
                 (apply create-image file-or-data type data-p props)))
        (apply orig-fn args)))

    (defadvice! org:fix-uuidgen (uuid)
      "TODO"
      :filter-return #'org-id-new
      (if (eq org-id-method 'uuid)
          (downcase uuid)
        uuid))

  (defun org:setup-smartparens ()
    (provide 'smartparens-org))

  ;; TODO
  (defun org:setup-keys ()
    ())

  :init
  (add-hook! 'org-load-hook
             #'org:setup-dirs
             #'org:setup-appearance
             #'org:setup-agenda
             #'org:setup-attachments
             #'org:setup-babel
             #'org:setup-capture
             #'org:setup-custom-links
             #'org:setup-export
             #'org:setup-habit
             #'org:setup-hacks
             ;;#'org-setup-keys
             #'org:setup-smartparens)
  :custom
  (org-archive-subtree-save-file-p . t)
  (org-id-locations-file-relative . t)
  :config
  (add-hook 'org-mode-local-vars-hook #'eldoc-mode)
  (add-hook 'org-mode-hook #'orgtbl-mode))

(use-package org-crypt
  :ensure nil
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :hook (org-reveal-start . org-decrypt-entry)
  :preface
  ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
  ;; is a better default than the empty string `org-crypt-key' defaults to.
  (defvar org-crypt-key nil)
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (add-hook! 'org-mode-hook
      (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

(use-package org-clock
  :ensure nil
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (concat my-etc-dir "org-clock-save.el"))
  :custom
  (org-clock-persist . 'history)
  (org-clock-in-resume . t)
  :config
  (defadvice! org-clock:lazy-load (&rest _)
    "Lazy load org-clock until its commands are used."
    :before '(org-clock-in
              org-clock-out
              org-clock-in-last
              org-clock-goto
              org-clock-cancel)
    (org-clock-load))
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package toc-org
  :hook (org-mode-hook . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))

  ;;;;; PDFTools
  ;;;;;

(use-package org-pdftools
  :when (package-installed-p 'pdftools)
  :commands org-pdftools-export
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters (or (bound-and-true-p org-pdftools-link-prefix) "pdf")
                             :follow #'org-pdftools-open
                             :complete #'org-pdftools-complete-link
                             :store #'org-pdftools-store-link
                             :export #'org-pdftools-export)
    (add-hook! 'org-open-link-functions
      (defun org-pdftools:open-legacy-pdf-links-fn (link)
        (let ((regexp "^pdf\\(?:tools\\|view\\):"))
          (when (string-match-p regexp link)
            (org-pdftools-open (replace-regexp-in-string regexp "" link))
            t))))))

;;;; LaTeX
;;;;

(use-package tex-mode
  :ensure nil
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-parse-self . t) ; parse on load
  (TeX-auto-save . t)  ; parse on save
  ;; use hidden dirs for auctex files
  (TeX-auto-local . ".auctex-auto")
  (TeX-style-local . ".auctex-style")
  (TeX-source-correlate-mode . t)
  (TeX-source-correlate-method . 'synctex)
  ;; don't start the emacs server when correlating sources
  (TeX-source-correlate-start-server . nil)
  ;; automatically insert braces after sub/superscript in math mode
  (TeX-electric-sub-and-superscript . t)
  ;; Use xelatex to support unicode
  (TeX-engine . 'xetex)
  (tex-command . "xelatex")
  (LaTeX-command . "xelatex"))

(use-package context
  :ensure nil
  :magic ("%!TEX TS-PROGRAM: context" . ConTeXt-mode))

;;;; Artist
;;;;

(use-package artist
  :ensure nil
  :preface
  ;; TODO need to be able to save art to file and clear scratch buffer
  ;; when finished
  (defun artist:switch ()
    "Switch to `*scratch*' in order to draw ASCII art."
    (interactive)
    (switch-to-buffer "*scratch*" nil t)
    (artist-mode)))

;;;; Skeleton
;;;; TODO make snippets

(use-package skeleton
  :ensure nil
  :demand t
  :config
  ;; Thank you reddit user b3n:
  ;; https://old.reddit.com/r/emacs/comments/ml4wql/weekly_tipstricketc_thread/gtkc524/
  (defmacro skeleton:global-snip (name &rest skeleton)
    "Create a global \"snippet\" with NAME and SKELETON.
NAME must be valid in the Emacs Lisp naming convention.

SKELETON must be a body that is valid to `Skeleton''s internal language.

This macro makes use of `define-skeleton' and `define-abbrev' in order to
create something similar to a code/writing snippet system, like that of
`YASnippet'. Keep in mind that all abbreviations created are put in the
`global-abbrev-table' under the named passed to this macro. That may or
may not be something you want, depending on your uses.
If you're looking to only define an abbrev for a specific file/mode, see
`skeleton:file-snip'."
    (declare (debug t))
    (let* ((snip-name (symbol-name `,name))
           (func-name (intern (concat "snip-" snip-name))))
      `(progn
         (define-skeleton ,func-name
           ,(concat snip-name " skeleton")
           ,@skeleton)
         (define-abbrev global-abbrev-table ,snip-name
           "" ',func-name))))

  (defmacro skeleton:file-snip (name mode &rest skeleton)
    "Create a MODES specific \"snippet\" with NAME and SKELETON.
NAME must be valid in the Emacs Lisp naming convention.

MODE must be a valid feature or file
(something acceptable by `eval-after-load').

MODE can be a list of features or files
(again, something acceptable by `eval-after-load').

SKELETON must be a body that is valid to `Skeleton''s internal language.
This macro makes use of `define-skeleton' and `define-abbrev' in order to
create something similar to a code/writing snippet system, like that of
`YASnippet'.

Keep in mind that all abbreviations created are put in the `local-abbrev-table'
under the named (MODE) passed to this macro. That may or may not be something
you want, depending on your uses. If you're looking to only define an abbrev
globally, see `skeleton:global-snip'."
    (declare (debug t))
    (let* ((snip-name (symbol-name `,name))
           (func-name (intern (concat "snip-" snip-name)))
           (mode-str (if (listp)
                         (mapconcat 'identity mode ", ")
                       (format "%s" mode))))
      `(cond ((symbolp ,mode)
              (define-skeleton ,func-name
                ,(format "%s %s %s." snip-name " skeleton. Defined in " mode-str)
                ,@skeleton)
              (eval-after-load ',mode
                (define-abbrev local-abbrev-table ,snip-name
                  "" ',func-name)))
             ((listp ,mode)
              (define-skeleton ,func-name
                ,(format "%s %s %s %s." snip-name " skeleton. Defined in " mode-str " modes/features")
                ,@skeleton)
              (dolist (m mode)
                (eval-after-load ',m
                  (define-abbrev local-abbrev-table ,snip-name
                    "" ',func-name))))))))

;;;; Markdown
;;;;

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))
  :custom
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-open-command
                         (cond (macos-p "open")
                               (linux-p "xdg-open")))
  (markdown-content-type  "application/xhtml+xml")
  (markdown-css-paths
                      '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                        "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (markdown-xhtml-header-content
                                 (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                                          "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                                          "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                                          "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                                          "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
  :config
  (advice-add #'markdown-match-generic-metadata :override (lambda (&rest _)
                                                            (ignore (goto-char (point-max)))))
  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))

  ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
  (setq-mode-local markdown-mode
                   fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                                fill-nobreak-predicate)))

;;;; Literate Calc
;;;;

(use-package literate-calc-mode)

(provide 'writing)
;;; writing.el ends here
