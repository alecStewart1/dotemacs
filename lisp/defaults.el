;;; defaults.el --- Some default settings for Emacs to mkae it more sane and faster -*- lexical-binding: t; -*-
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
;;  Making Emacs a little more sane out-of-the-box.
;;
;;  A lot of these are things we need to fix/hack before we
;;  add in anything else.
;;
;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'lib)

;;;; Emacs itself
;;;;

(use-package emacs
  :ensure nil
  :preface
  (defun minibuffer:crm-indicator (args)
    "Add a prompt for `completing-read-multiple'."
    (cons (concat "[CRM] " (car args) (cdr args))))
  :init
  (if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars))
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

  ;;; Things Emacs disables for some reason
  ;;;
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page   'disabled nil)
  (put 'upcase-region    'disabled nil)
  (put 'downcase-region  'disabled nil)
  (put 'list-threads     'disabled nil)

  ;;(fset 'x-popup-menu #'ignore)
  (fset #'display-startup-echo-area-message #'ignore)
  (fset #'yes-or-no-p #'y-or-n-p)
  
  (if emacs27-p
      (progn
        (setq bidi-inhibit-bpa t)
        (setq-default bidi-display-reordering 'left-to-right
                      bidi-paragraph-direction 'left-to-right)))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (advice-add 'completing-read-multiple :filter-args #'minibuffer:crm-indicator)
    :custom
  ;; Startup
  (inhibit-startup-screen            t)
  (inhibit-startup-message           t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-default-init              t)
  (initial-major-mode                'fundamental-mode)
  (initial-scratch-message           nil)

  ;; Scrolling
  (hscroll-margin 1)
  (hscroll-step 1)
  (scroll-step 1)
  (redisplay-dont-pause t)
  (scroll-conservatively 10000)
  (scroll-up-aggressively  0.01)
  (scroll-down-aggressively 0.01)
  (fast-but-imprecise-scrolling t)
  (auto-window-vscroll nil)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  (jit-lock-defer-time 0)

  ;; Cursor and Mouse
  (visible-cursor nil)
  (x-stretch-cursor nil)
  (cursor-in-non-selected-windows nil)
  (mouse-wheel-scroll-amount '(5 ((shift) . 2)))
  (mouse-wheel-progressive-speed nil)
  (mouse-yank-at-point t)

  ;; Window, frame, minibuffer
  (display-line-numbers-width 3)
  (enable-recursive-minibuffers t)
  (frame-inhibit-implied-resize t)
  (fringe-indicator-alist
   (delq (assq 'continuation fringe-indicator-alist)
         fringe-indicator-alist))
  (highlight-nonselected-windows nil)
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  (max-mini-window-height 0.15)
  (resize-mini-windows 'grow-only)
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (frame-title-format '("%b - Emacs"))
  (icon-title-format frame-title-format)

  ;; Don't use some GUI stuff
  (use-file-dialog nil)
  (use-dialog-box nil)
  (x-gtk-use-system-tooltips nil)

  ;; No noise, pls
  (ring-bell-function #'ignore)
  (visible-bell nil)

  ;; Keystrokes
  (echo-keystrokes 0.02)

  ;; Text
  (x-underline-at-descent-line t)
  (truncate-lines t)
  (truncate-partial-width-windows 50)
  (sentence-end-double-space nil)

  ;; Fill and wrap
  (fill-column 100)
  (word-wrap t)
  (adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (adaptive-fill-first-line-regexp "^* *$")

  ;; Whitespace
  (show-trailing-whitespace nil)

  ;; UTF-8, pls
  (locale-coding-system 'utf-8)

  ;; No tabs, only spaces
  (indent-tabs-mode nil)
  (tab-width 4)

  ;; Autosave
  (auto-save-list-file-prefix (concat my-cache-dir "autosave/"))
  (auto-save-include-big-deletions t)
  (auto-save-file-name-transformers
   (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
     ;; Prefix tramp autosaves to prevent conflicts with local ones
               (concat auto-save-list-file-prefix "tramp-\\2") t)
         (list ".*" auto-save-list-file-prefix t)))

  ;; Don't clutter my Emacs directory
  (shared-game-score-directory        (concat my-etc-dir "shared-game-score/"))
  (gamegrid-user-score-file-directory (concat my-etc-dir "games/"))
  (request-storage-directory          (concat my-cache-dir "request/"))

  ;; Misc.
  (apropos-do-all  t)
  (image-animate-loop  t)
  (show-help-function  nil)
  (history-length  500)
  (debug-on-quit  nil)
  (debug-on-error  nil)
  (delete-by-moving-to-trash  t)
  (create-lockfiles  nil)
  (autoload-compute-prefixes  nil)
  (load-prefer-newer  t)
  (mode-line-default-help-echo nil)
  (custom-unlispify-menu-entries nil)
  (custom-unlispify-tag-names nil)
  (read-process-output-max (* 2 1024 1024))
  :config
  (setq-default
   native-comp-deferred-compilation nil ; this can be unwanted
   comp-speed                       2
   ediff-window-setup-function      'ediff-setup-windows-plain
   jka-compr-verbose                nil    ; silence compression messages
   ffap-machine-p-known            'reject ; don't ping things that look like domain names
   vc-follow-symlinks              t
   tabify-regexp                   "^\t* [ \t]+"
   abbrev-mode                     t)

  ;; Abbreviations
  (setq abbrev-file-name (concat my-local-dir "abbrev.el")
        save-abbrevs     'silent) ; God, stop asking.

  ;; Nicer scrolling
  (when emacs29-p
    (pixel-scroll-precision-mode +1)
    (setq pixel-scroll-precision-large-scroll-height 40.0
          pixel-scroll-precision-interpolation-factor 30
          pixel-scroll-precision-use-momentum t))

  ;; Don’t let the mouse clash with the cursor.
  (mouse-avoidance-mode 'exile)

  ;; Authentication server directory
  (setq server-auth-dir  (concat my-cache-dir "server/"))

  ;; Unicode, pls
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (unless windows-nt-p
    (set-selection-coding-system 'utf-8))
  (modify-coding-system-alist 'process "*" 'utf-8)

  (with-eval-after-load 'eshell
    (setq hscroll-margin 0)
    (setenv "PAGER" "cat"))
  (with-eval-after-load 'shell
    (setq hscroll-margin 0)
    (setenv "PAGER" "cat"))
  (eval-after-load 'term
    (setq hscroll-margin 0))

  ;; MacOS stuff
  (when macos-p
    (setq locate-command "mdfind"
          ns-use-native-fullscreen nil
          ns-pop-up-frames nil
          mac-redisplay-dont-reset-vscroll t
          mac-mouse-wheel-smoothscroll nil
          mac-command-modifier      'super
          ns-command-modifier       'super
          mac-option-modifier       'meta
          ns-option-modifier        'meta
          mac-right-option-modifier 'none
          ns-right-option-modifier  'none
          exec-path (append exec-path '("/usr/local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

    (and (or (daemonp)
             (display-graphic-p))
         (require 'ns-auto-titlebar nil t)
         (ns-auto-titlebar-mode +1))

    ;; From Doom Emacs:
    ;; https://github.com/hlissner/doom-emacs/blob/af7c1d79bd63d78410aafc410d52ee5c1109ec26/modules/os/macos/config.el#L37
    ;; HACK On MacOS, disabling the menu bar makes MacOS treat Emacs as a
    ;;      non-application window -- which means it doesn't automatically capture
    ;;      focus when it is started, among other things, so enable the menu-bar for
    ;;      GUI frames, but keep it disabled in terminal frames because there it
    ;;      activates an ugly, in-frame menu bar.
    (add-hook! '(window-setup-hook after-make-frame-functions)
      (defun macos:init-menu-bar-in-gui-frames (&optional frame)
        "Re-enable menu-bar-lines in GUI frames."
        (when-let (frame (or frame (selected-frame)))
          (when (display-graphic-p frame)
            (set-frame-parameter frame 'menu-bar-lines 1)))))

    (with-eval-after-load 'auth-source
      (pushnew! auth-sources 'macos-keychain-internet 'macos-keychain-generic))))

;;;; Compiling things
;;;;


(use-package bytecomp
  :ensure nil
  :custom
  (byte-compile-verbose        nil)
  (byte-compile-warnings       '(not free-vars unresolved noruntime lexical make-local))
  (async-byte-compile-log-file (concat my-etc-dir "async-bytecomp.log")))

(use-package comp
  :ensure nil
  :init
  (setq-default
   native-comp-compiler-options '("-O2" "-mtune=native"))
  :config
  (mapc (apply-partially #'add-to-list 'native-comp-deferred-compilation-deny-list)
        (let ((local-dir-re (concat "\\`" (regexp-quote my-local-dir))))
          (list (concat local-dir-re ".*/with-editor\\.el\\’")))))

(use-package pcache
  :ensure nil
  :defer t
  :custom
  (pcache-directory (concat my-cache-dir "pcache/")))

;;;; Simple
;;;;

(use-package simple
  :ensure nil
  :hook ((org-mode prog-mode text-mode) . auto-fill-mode)
  :custom
  (idle-update-delay                   1.2)
  (track-eol                           t)
  (completion-show-help                nil)
  (column-number-mode                  t)
  (line-number-mode                    t)
  (kill-do-not-save-duplicates         t)
  (save-interprogram-paste-before-kill t)
  (shift-select-mode                   t)
  (delete-trailing-lines               nil)
  (set-mark-command-repeat-pop         t)
  (kill-ring-max                       30000))

;;;; Advice
;;;;

(use-package advice
  :ensure nil
  :custom
  ;; silence redefined function warnings
  (ad-redefinition-action 'accept))

;;;; Windows to the Walls
;;;;


(use-package window
  :ensure nil
  :init
  ;; Leave me alone
  (add-to-list 'display-buffer-alist '("^\\*Warnings\\*$"
                                       (display-buffer-no-window)
                                       (allow-no-window . t)))
  (add-to-list 'display-buffer-alist '("^\\*Compile-log\\*$"
                                       (display-buffer-no-window)
                                       (allow-no-window . t)))
  (add-to-list 'display-buffer-alist '("^\\*Flycheck errors\\*$"
                                       (display-buffer-no-window)
                                       (allow-no-window . t)))
  :custom
  (pop-up-windows nil)
  ;; favor horizontal splits
  (split-width-threshold 160))

(use-package frame
  :ensure nil
  :hook (emacs-startup . window-divider-mode)
  :bind
  ("C-z" . nil)
  :init
  (blink-cursor-mode -1)
  :custom
  (window-divider-default-places       t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width  1)
  :config
  ;; for fullscreen
  (cond
   (windows-nt-p (set-frame-parameter nil 'fullscreen 'fullboth))
   (t            (set-frame-parameter nil 'fullscreen 'maximized))))

(use-package wid-edit
  :ensure nil
  :custom
  (widget-image-enable nil))

;;;; Cursor
;;;;

(use-package cursor-sensor
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode))

;;;; Editing Text
;;;;

(use-package select
  :ensure nil
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package delsel
  :ensure nil
  :hook (pre-command . delete-selection-mode))

(use-package whitespace
  :ensure nil
  :init
  (put 'whitespace-toggle-options 'disabled t)
  (put 'global-whitespace-toggle-options 'disabled t))

;;;; Files
;;;;

;;;;; Hooks and advice related to files
;;;;;

(add-hook! 'find-file-not-found-functions
  (defun files:create-missing-directories ()
    "Automatically create missing directories when creatign new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; HACK
(defadvice! files:make-hashed-auto-save-file-name (orig-fn)
  "Compress the auto-save file name so paths don't get too long."
  :around #'make-auto-save-file-name
  (let ((buffer-file-name
         (if (or
              (null buffer-file-name)
              (find-file-name-handler buffer-file-name
                                      'make-auto-save-file-name))
             buffer-file-name
           (sha1 buffer-file-name))))
    (funcall orig-fn)))

;; HACK
(defadvice! files:make-hashed-backup-file-name (orig-fn file)
  "A few places use the backup file name so paths don't get too long."
  :around #'make-backup-file-name-1
  (let ((alist backup-directory-alist)
        backup-directory)
    (while alist
      (let ((elt (pop alist)))
        (if (string-match (car elt) file)
            (setq backup-directory (cdr elt)
                  alist nil))))
    (let ((file (funcall orig-fn file)))
      (if (or (null backup-directory)
              (not (file-name-absolute-p backup-directory)))
          file
        (expand-file-name (sha1 (file-name-nondirectory file))
                          (file-name-directory file))))))

;;;;; Package itself
;;;;;

(use-package files
  :ensure nil
  :custom
  (backup-directory-alist (list (cons "." (concat my-cache-dir "backup/"))))
  (large-file-warning-threshold          15000000)
  (confirm-nonexistent-file-or-buffer    t)
  (auto-mode-case-fold                   nil)
  (require-final-newline                 t)
  (make-backup-files                     nil)
  (auto-save-default                     t)
  ; resolve symlinks when opening files
  (find-file-visit-truename              t)
  (find-file-suppress-same-file-warnings t)
  (delete-old-versions                   t)
  (backup-by-copying                     t)
  (version-control                       t)
  (delete-old-versions                   t)
  (kept-old-versions                     5)
  (kept-new-versions                     5)
  :config
  (defun sudo-file-path (file)
    (let ((host (or (file-remote-p file 'host) "localhost")))
      (concat "/" (when (file-remote-p file)
                    (concat (file-remote-p file 'method) ":"
                            (if-let (use (file-remote-p file 'user))
                                (concat user "@" host)
                              host)
                            "|"))
              "sudo:root@" host
              ":" (or (file-remote-p file 'localname)
                      file))))

  (defun sudo-find-file (file)
    (interactive "FOpen file as root: ")
    (find-file (sudo-file-path file))))

;;;; Searching and Replacing
;;;;

(use-package grep
  :ensure nil
  :defer t
  :custom
  (grep-command (executable-find "rg"))
  (find-program (executable-find "fd")))

(use-package isearch
  :ensure nil
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-r" . isearch-backward-regexp)
   ("C-M-r" . isearch-backward))
  :hook
  (isearch-update-post . isearch:aim-beginning)
  :preface
  ;; From: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#navigation-search
  (defun isearch:aim-beginning ()
    "Move cursor back to the beginning of the current match."
    (when (and isearch-forward (number-or-marker-p isearch-other-end))
      (goto-char isearch-other-end)))
  :custom
  (isearch-allow-scroll t)
  (lazy-highlight-buffer t)
  (lazy-highlight-cleanup nil)
  (lazy-highlight-initial-delay 0))

(use-package replace
  :ensure nil
  :bind
  (("M-%" . query-replace-regexp)
   ("C-M-%" . query-replace))
  :config
  (when (require 'dash nil t)
    ;; From: https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el#L1484
    (defun replace:query-replace-rx (&rest _)
      "Call `query-replace-regexp', reading regexp in `rx' syntax.
Automatically wraps in parens and adds `seq' to the beginning of
the form."
      (interactive)
      (cl-letf (((symbol-function #'query-replace-read-from) (lambda (&rest _)
                                                               (--> (read-string "rx form: ")
                                                                    (concat "'(seq " it ")")
                                                                    (read it)
                                                                    (cadr it)
                                                                    (rx-to-string it)))))
        (call-interactively #'query-replace-regexp)))))

;;;; Security things
;;;;

(use-package nsm
  :ensure nil
  :demand t
  :custom
  (network-security-level 'high))

(use-package gnutls
  :ensure nil
  :demand t
  :init
  (setq gnutls-verify-error (not (getenv "INSECURE"))
        gnutls-min-prime-bits 3072
        gnutls-algorithm-priority
        (when (boundp 'libgnutls-version)
          (concat "SECURE128:+SECURE192:-VERS-ALL"
                  (if (and (not windows-nt-p)
                           (not (version< emacs-version "26.3"))
                           (>= libgnutls-version 30605))
                      ":+VERS-TLS1.3")
                  ":+VERS-TLS1.2")))
  :custom
  (tls-checktrust gnutls-verify-error)
  (tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                 "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
      --strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                 ;; compatibility fallbacks
                 "gnutls-cli -p %p %h")))

(use-package auth-source
  :ensure nil
  :demand t
  :init
  (setq auth-sources
        (list (concat my-etc-dir "authinfo.gpg") "~/.authinfo.gpg")))

;;;; Misc.
;;;;

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style nil))

(use-package ansi-color
  :ensure nil
  :custom
  (ansi-color-for-comint-mode t))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (concat my-etc-dir "bookmarks"))
  (bookmark-save-flag    t))

(use-package grep
  :ensure nil
  :defer t)

(use-package url
  :ensure nil
  :custom
  (url-configuration-directory (concat my-etc-dir "url/"))
  (url-cache-directory         (concat my-cache-dir "url/")))

(use-package tramp
  :ensure nil
  :custom
  (tramp-auto-save-directory (concat my-cache-dir "tramp-auto-save/"))
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-persistency-file-name (concat my-cache-dir "tramp-persistency.el")))

(use-package kmacro
  :ensure nil
  :bind
  (("C-x C-k c" . kmacro-call-macro)
   ("C-x C-k S" . kmacro-end-macro)
   ("C-x C-k v" . kmacro-view-macro)))

(use-package misc
  :ensure nil
  :bind (("M-z" . zap-up-to-char)))

(eval-after-load 'tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

(provide 'defaults)
;;; defaults.el ends here
