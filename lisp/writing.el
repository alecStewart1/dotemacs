;;; writing.el --- Writing and taking notes in Emacs -*- lexical-binding: t; -*-
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
  :ensure org-contrib
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
     org-agenda-deadline-faces
     '((1.0001 . error)
       (1.0 . org-warning)
       (0.5 . org-upcoming-deadline)
       (0.0 . org-upcoming-distant-deadline))
     org-agenda-window-setup 'current-window
     org-agenda-skip-unavailable-files t
     org-agenda-span 10
     org-agenda-start-on-weekday nil
     org-agenda-start-day "-3d"
     org-agenda-files '("~/Documents/Org/agenda.org"
                        "~/Documents/Org/Notes/personal-fitness.org")
     org-agenda-inhibit-startup t))

  (defun org:setup-appearance ()
    (setq org-indirect-buffer-display 'current-window
          org-eldoc-breadcrumb-separator " -> "
          org-enforce-todo-dependencies t
          org-entities-user
          '(("flat" "\\flat" nil "" "" "266D" "")
            ("sharp" "\\sharp" nil "" "" "266F" ""))
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t
          org-fontify-whole-heading-line t
          org-hide-emphasis-markers t
          org-footnote-auto-label t
          org-hide-leading-stars t
          org-image-actual-width nil
          org-imenu-depth 6
          org-priority-faces
          '((?A . error)
            (?B . warning)
            (?C . success))
          org-startup-indented t
          org-startup-truncated nil
          org-startup-folded nil
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
             "IDEA(i)"
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
        (format "Link: %s" link))))

  (defun org:setup-babel ()
    (setq org-src-preserve-indentation t
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-src-window-setup 'other-window
          org-edit-src-content-indentation 0
          org-edit-src-persistent-message nil
          org-confirm-babel-evaluate nil
          org-babel-lisp-eval-fn #'sly-eval
          org-link-elisp-confirm-function nil)

    ;; (eval-after-load 'ob
    ;;   (add-to-list 'org-babel-default-lob-header-args '(:sync)))

    (defadvice! org-babel:fix-newline-and-indent-in-src-blocks (&optional indent _arg _interactive)
      "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
      :after #'org-return
      (when (and indent
                 org-src-tab-acts-natively
                 (org-in-src-block-p t))
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command))))

    (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

    (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit))

  (defun org:setup-capture ()
    (use-package org-capture
      :ensure nil
      :commands org-capture
      :config
      (org-capture-put :kill-buffer t)
      (add-hook 'org-after-refile-insert-hook #'save-buffer)
      (defadvice! org-capture:expand-var-file (file)
        "TODO"
        :filter-args #'org-capture-expand-file
        (if (and (symbolp file) (boundp file))
            (expand-file-name (symbol-value file) org-directory)
          file))

      (if (package-installed-p 'evil)
          (add-hook 'org-capture-mode-hook #'evil-insert-state))
      (if (package-installed-p 'meow)
          (add-hook 'org-capture-mode-hook #'meow-insert-mode))

      (setq org-capture-templates
            (append org-capture-templates
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
                                               "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))))))

  (defun org:setup-attachments ()
    (setq org-attach-store-link-p t
          org-attach-use-inheritance t)

    (use-package org-attach
      :ensure nil
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
              '("wolfram"     . "https://wolframalpha.com/input/?i=%s"))

    ;; (use-package org-yt
    ;;   :init
    ;;   (advice-add org-yt-image-data-fun :before-while (lambda (&rest _)
    ;;                                                     (not (eq org-display-remote-inline-images 'skip)))))
    )

  (defun org:setup-export ()
    (setq org-export-with-smart-quotes t
          org-html-validation-link nil
          org-latex-prefer-user-labels t
          org-export-with-toc nil)
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
    (add-to-list 'org-file-apps '(remote . emacs))

    (advice-add #'org-link--open-help :around #'use-helpful)

    (defadvice! org:recenter-after-follow-link (&rest _args)
      "Recenter after following a link, but only internal or file links."
      :after '(org-footnote-action
               org-follow-timestamp-link
               org-link-open-as-file
               org-link-search)
      (when (get-buffer-window)
        (recenter)))

    (with-eval-after-load 'org-eldoc
      (puthash "org" #'ignore org-eldoc-local-functions-cache)
      (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
      (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

    (defadvice! org:show-parents (&optional arg)
      "Show all headlines in the buffer, like a table of contents.
With numerical argument N, show content up to level N."
      :override #'org-content
      (interactive "p")
      (org-show-all '(headings drawers))
      (save-excursion
        (goto-char (point-max))
        (let ((regexp (if (and (wholenump arg) (> arg 0))
                          (format "^\\*\\{%d,%d\\} " (1- arg) arg)
                        "^\\*+ "))
              (last (point)))
          (while (re-search-backward regexp nil t)
            (when (or (not (wholenump arg))
                      (= (org-current-level) arg))
              (org-flag-region (line-end-position) last t 'outline))
            (setq last (line-end-position 0))))))

    (defadvice! org:strip-properties-from-outline (fn &rest args)
      "Fix variable height faces in eldoc breadcrumbs."
      :around #'org-format-outline-path
      (let ((org-level-faces
             (cl-loop for face in org-level-faces
                      collect `(:foreground ,(face-foreground face nil t)
                                :weight bold))))
        (apply fn args)))

    (defadvice! org:export-agenda-from-recentf (orig-fn file)
      "TODO"
      :around #'org-get-agenda-file-buffer
      (let ((recentf-exclude (list (lambda (_file) t))))
        (funcall orig-fn file)))

    (defvar recentf-exclude)
    (defadvice! org:optimize-backgrounded-agenda-buffers (fn file)
      "Prevent temporarily opened agenda buffers from polluting recentf."
      :around #'org-get-agenda-file-buffer
      (let ((recentf-exclude (list (lambda (_file) t)))
            org-startup-indented
            org-startup-folded
            vc-handled-backends
            org-mode-hook
            find-file-hook)
        (funcall fn file)))

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
        uuid)))

  (defun org:setup-smartparens ()
    (provide 'smartparens-org))


  (defun org:element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'.

Taken from: https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el"
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (org:element-descendant-of type parent))))

  (defun org:return-dwim (&optional default)
    "A helpful replacement for `org-return'.  With prefix, call `org-return'.
On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table.

Taken from: https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el"

    (interactive "P")
    (if default
        (org-return)
      (cond
       ;; Act depending on context around point.

       ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
       ;; followed.

       ;; ((eq 'link (car (org-element-context)))
       ;;  ;; Link: Open it.
       ;;  (org-open-at-point-global))

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (when (org-at-heading-p)
                   ;; At the same heading
                   (forward-line)
                   (insert "\n")
                   (forward-line -1))
                 ;; FIXME: looking-back is supposed to be called with more arguments.
                 (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
                   (insert "\n"))
                 (forward-line -1)))))

       ((org-at-item-checkbox-p)
        ;; Checkbox: Insert new item with checkbox.
        (org-insert-todo-heading nil))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context))  ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (org:element-descendant-of 'item context))  ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (org-insert-item)
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return))
              (t
               ;; Non-empty row: call `org-return'.
               (org-return))))
       (t
        ;; All other cases: call `org-return'.
        (org-return)))))


  (defun org:indent-maybe ()
    "From Doom Emacs
Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode."
    (interactive)
    (cond ((not (and (bound-and-true-p evil-local-mode)
                     (evil-insert-state-p)))
           nil)
          ((not (bound-and-true-p meow-insert-mode))
           nil)
          ((org-at-item-p)
           (if (eq this-command 'org-shifttab)
               (org-outdent-item-tree)
             (org-indent-item-tree))
           t)
          ((org-at-heading-p)
           (ignore-errors
             (if (eq this-command 'org-shifttab)
                 (org-promote)
               (org-demote)))
           t)
          ((org-in-src-block-p t)
           (org-babel-do-in-edit-buffer
            (call-interactively #'indent-for-tab-command))
           t)
          ((and (save-excursion
                  (skip-chars-backward " \t")
                  (bolp))
                (org-in-subtree-not-table-p))
           (call-interactively #'tab-to-tab-stop)
           t)))

  ;; TODO figure out some local prefix/leader key.
  (defun org:setup-keys ()
    (setq org-special-ctrl-a/e t
          org-M-RET-may-split-line nil
          ;; insert new headings after current subtree rather than inside it
          org-insert-heading-respect-content t)

    (add-hook 'org-tab-first-hook #'org:indent-maybe)

    ;; Global Keys
    (global-set-key (kbd "C-c c") #'org-capture)
    (global-set-key (kbd "C-c a") #'org-agenda)

    ;; Org-Mode keys
    (define-key org-mode-map (kbd "TAB") #'org-cycle)
    (define-key org-mode-map (kbd "<tab>") #'org-cycle)
    (define-key org-mode-map (kbd "RET") #'org:return-dwim)
    (define-key org-mode-map (kbd "<return>") #'org:return-dwim)
    (define-key org-mode-map (kbd "C-S-RET") #'org-insert-subheading)
    (define-key org-mode-map (kbd "<C-S-return>") #'org-insert-subheading))

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
             #'org:setup-keys
             #'org:setup-smartparens)
  :custom
  (org-archive-subtree-save-file-p t)
  (org-id-locations-file-relative t)
  :config
  (add-hook 'org-mode-local-vars-hook #'eldoc-mode)
  (add-hook 'org-mode-hook #'orgtbl-mode)

  ;; TODO this don’t work
  ;; (mode-snippet org-block org-mode
  ;;   "Block type: "
  ;;   ?\n "#+begin_" str
  ;;   ?\n _ ?\n
  ;;   "#+end_" str ?\n)

  ;; TODO but this do
  (define-skeleton org-block-skeleton
    "Skeleton for Org-Mode to create special blocks.
The #+begin_ .. #+end_ blocks"
    "Block type: "
    ?\n "#+begin_" str
    ?n _ ?\n
    "#+end_" str ?\n)
  (define-key org-mode-map (kbd "C-c b") #'org-block-skeleton))

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
  :config
  (defadvice! org-clock:lazy-load (&rest _)
    "Lazy load org-clock until its commands are used."
    :before '(org-clock-in
              org-clock-out
              org-clock-in-last
              org-clock-goto
              org-clock-cancel)
    (org-clock-load))
  (add-hook 'kill-emacs-hook #'org-clock-save)
  :custom
  (org-clock-persist 'history)
  (org-clock-in-resume t))

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
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  ;; Fix #1849: allow fill-paragraph in itemize/enumerate
  (defadvice latex:re-indent-itemize-and-enumerate
      (around LaTeX-fill-region-as-para-do (fn &rest args) activate)
    "TODO"
    (let ((LaTeX-indent-environment-list
           (append LaTeX-indent-environment-list
                   '(("itemize"   +latex-indent-item-fn)
                     ("enumerate" +latex-indent-item-fn)))))
      (apply fn args)))

  (defadvice latex:dont-indent-itemize-and-enumerate
      (around LaTeX-fill-region-as-paragraph (fn &rest args) activate)
    "TODO"
    (let ((LaTeX-indent-environment-list LaTeX-indent-environment-list))
      (delq! "itemize" LaTeX-indent-environment-list 'assoc)
      (delq! "enumerate" LaTeX-indent-environment-list 'assoc)
      (apply fn args)))

  (setq-mode-local latex-mode
                   company-backends '(company-capf
                                      company-auctex-environments
                                      compnay-auctex-macros))
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  ;; use hidden dirs for auctex files
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  ;; don't start the emacs server when correlating sources
  (TeX-source-correlate-start-server nil)
  ;; automatically insert braces after sub/superscript in math mode
  (TeX-electric-sub-and-superscript t)
  ;; Use xelatex to support unicode
  (TeX-engine 'xetex)
  (tex-command "xelatex")
  (LaTeX-command "xelatex")
  (LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-toc
                        LaTeX-section-section
                        LaTeX-section-label))
  (LaTeX-fill-break-at-separators nil)
  (LaTeX-item-indent 0))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode   . org-cdlatex-mode)
  :config
  ;; These mess with a lot stuff in auctex
  (define-key cdlatex-mode-map (kbd "(") nil)
  (define-key cdlatex-mode-map (kbd "{") nil)
  (define-key cdlatex-mode-map (kbd "[") nil)
  (define-key cdlatex-mode-map (kbd "|") nil)
  (define-key cdlatex-mode-map (kbd "TAB") nil)
  (define-key cdlatex-mode-map (kbd "<tab>") nil)
  (define-key cdlatex-mode-map (kbd "^") nil)
  (define-key cdlatex-mode-map (kbd "_") nil)
  (define-key cdlatex-mode-map [(control return)] nil)
  (define-key cdlatex-mode-map (kbd "C-RET") nil)
  (define-key cdlatex-mode-map (kbd "<C-return>") nil))

;;;; SES: Simple Emacs Spreadsheet
;;;;

(use-package ses
  :ensure nil
  :mode ("\\.ses\\'" . ses-mode)
  :config
  ;; Pull from the Emacs Wiki:
  ;; http://emacswiki.org/emacs/SimpleEmacsSpreadsheet
  (defun ses:read-from-csv-file (file)
    "Insert the contents of a CSV file named FILE into the current position."
    (interactive "fCSV file: ")
    (let ((buf (get-buffer-create "*ses-csv*"))
          text)
      (save-excursion
        (set-buffer buf)
        (erase-buffer)
        ;; TODO what in God's name is this?
        (process-file "ruby" file buf nil "-e" "require 'csv'; CSV::Reader.parse(STDIN) { |x| puts x.join(\"\\t\") }")
        (setq text (buffer-substring (point-min) (point-max))))
      (ses-yank-tsf text nil)))

  (defun ses:write-to-csv-file (file)
    "Write the values of the current buffer into a CSV file named FILE."
    (interactive "FCSV file: ")
    (push-mark (point-min) t t)
    (goto-char (- (point-max) 1))
    (ses-set-curcell)
    (ses-write-to-csv-file-region file))

  (defun ses:write-to-csv-file-region (file)
    "Write the values of the region into a CSV file named FILE."
    (interactive "FCSV file: ")
    (ses-export-tab nil)
    (let ((buf (get-buffer-create "*ses-csv*")))
      (save-excursion
        (set-buffer buf)
        (erase-buffer)
        (yank)
        ;; TODO why?
        ;; TODO redo this?
        (call-process-region
         (point-min)
         (point-max)
         "ruby" t buf nil "-e" "require 'csv'; w = CSV::Writer.create(STDOUT); STDIN.each { |x| w << x.chomp.split(/\\t/) }")
        (write-region (point-min) (point-max) file)))))

;;;; Artist
;;;;

(use-package artist
  :ensure nil
  :config
  ;; TODO need to be able to save art to file and clear scratch buffer
  ;; when finished
  (defun artist:switch ()
    "Switch to `*scratch*' in order to draw ASCII art."
    (interactive)
    (switch-to-buffer "*scratch*" nil t)
    (artist-mode)))

;;;; Markdown, the **inferior** markup format
;;;;

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))
  :config
  (advice-add #'markdown-match-generic-metadata :override (lambda (&rest _)
                                                            (ignore (goto-char (point-max)))))
  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))

  ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
  (setq-mode-local markdown-mode
                   fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                                fill-nobreak-predicate))
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
                                          "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")))

;;;; Literate Calc
;;;;

(use-package literate-calc-mode)

(provide 'writing)
;;; writing.el ends here
