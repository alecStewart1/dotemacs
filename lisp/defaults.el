;;; defaults.el --- Some default settings for Emacs to mkae it more sane and faster -*- lexical-binding: t; -*-
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
;;  Making Emacs a little more sane out-of-the-box.
;;
;;  A lot of these are things we need to fix/hack before we
;;  add in anything else.
;;
;;  Here you'll see I make use of `leaf', and it's
;;  `:custom' key to customize variables, as well as to defer certain packages.
;;
;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'lib)

;;;; Emacs itself
;;;;

(leaf emacs
  :doc "A lot of default settings for things in Emacs's C sources"
  :tag "builtin" "defaults"
  ;:preface
  ;(defun minibuffer:crm-indicator (args)
  ;  "Add a prompt for `completing-read-multiple'."
  ;  (cons (concat "[CRM] " (car args) (cdr args))))
  :init
  (if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars))
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (fset 'x-popup-menu #'ignore)
  (if emacs27-p
      (progn
        (setq bidi-inhibit-bpa t)
        (setq-default bidi-display-reordering 'left-to-right
                      bidi-paragraph-direction 'left-to-right)))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;:advice
  ;(:filter-args completing-read-multiple minibuffer:crm-indicator)
  :custom
  ;; Scrolling
  `((hscroll-margin . 1)
    (hscroll-step . 1)
    (scroll-conservatively . 10000)
    (scroll-up-aggressively  . 0.01)
    (scroll-down-aggressively . 0.01)
    (fast-but-imprecise-scrolling . t)
    (auto-window-vscroll . nil)
    (scroll-margin . 0)
    (scroll-preserve-screen-position . t)
    ;; Cursor
    (visible-cursor . nil)
    (x-stretch-cursor . nil)
    (cursor-in-non-selected-windows . nil)
    ;; Window, frame, minibuffer
    (display-line-numbers-width . 3)
    (enable-recursive-minibuffers . t)
    (frame-inhibit-implied-resize . t)
    (highlight-nonselected-windows . nil)
    (indicate-buffer-boundaries . nil)
    (indicate-empty-lines . nil)
    (max-mini-window-height . 0.15)
    (resize-mini-windows . 'grow-only)
    (window-resize-pixelwise . t)
    (frame-resize-pixelwise . t)
    (minibuffer-prompt-properties
          '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
    (frame-title-format . '("%b - Emacs"))
    (icon-title-format frame-title-format)
    ;; Don't use some GUI stuff
    (use-file-dialog . nil)
    (use-dialog-box . nil)
    (x-gtk-use-system-tooltips . nil)
    ;; No noise, pls
    (ring-bell-function . #'ignore)
    (visible-bell . nil)
    ;; Keystrokes
    (echo-keystrokes . 0.02)
    ;; Text
    (x-underline-at-descent-line . t)
    (truncate-lines . t)
    (truncate-partial-width-windows . 50)
    ;; Fill and wrap
    (fill-column . 80)
    (word-wrap . t)
    ;; Whitespace
    (show-trailing-whitespace . nil)
    ;; UTF-8, pls
    (locale-coding-system . 'utf-8)
    ;; No tabs, only spaces
    (indent-tabs-mode . nil)
    (tab-width . 4)
    ;; Don't clutter my Emacs directory
    (auto-save-file-name-transformers .   ,(concat my-cache-dir "auto-save-list/"))
    (auto-save-list-file-name .           ,(concat my-cache-dir "autosave"))
    (shared-game-score-directory .        ,(concat my-etc-dir "shared-game-score/"))
    (gamegrid-user-score-file-directory . ,(concat my-etc-dir "games/"))
    (request-storage-directory .          ,(concat my-cache-dir "request/"))
    ;; Misc.
    (apropos-do-all . t)
    (image-animate-loop . t)
    (show-help-function . nil)
    (history-length . 500)
    (debug-on-quit . nil)
    (debug-on-error . nil)
    (delete-by-moving-to-trash . t)
    (create-lockfiles . nil)
    (autoload-compute-prefixes . nil)
    (load-prefer-newer . t)
    ;; TODO might change in future
    (mode-line-format . nil))
  :setq
  `((abbrev-file-name . ,(concat my-local-dir "abbrev.el"))
    (server-auth-dir .  ,(concat my-cache-dir "server/")))
  :setq-default
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (jka-compr-verbose .           nil) ; silence compression messages
  (ffap-machine-p-known .        'reject) ; don't ping things that look like domain names
  (vc-follow-symlinks .          t)
  (tabify-regexp .              "^\t* [ \t]+")
  :config
  (setq fringe-indicator-alist
        (delq (assq 'continuation fringe-indicator-alist)
             fringe-indicator-alist))
  ;; Unicode, pls
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (set-buffer-file-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
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
    (setq hscroll-margin 0)))

;;;; Compiling things
;;;;

(leaf *compliation
  :doc "For compling Emacs and other things."
  :config
  (leaf bytecomp
    :tag "builtin" "defaults"
    :leaf-defer nil
    :custom
    `((byte-compile-verbose .        nil)
      (byte-compile-warnings .       '(not free-vars unresolved noruntime lexical make-local))
      (async-byte-compile-log-file . ,(concat my-etc-dir "async-bytecomp.log"))))

  (leaf pcache
    :tag "builtin" "defaults"
    :custom
    `((pcache-directory . ,(concat my-cache-dir "pcache/")))))

;;;; Startup
;;;;

(leaf startup
  :tag "builtin" "defaults"
  :init
  (fset #'display-startup-echo-area-message #'ignore)
  :custom
  ;; be quiet at startup; don't load or display anything unnecessary
  (inhibit-startup-screen .            t)
  (inhibit-startup-message .           t)
  (inhibit-startup-echo-area-message . t)
  (inhibit-startup-echo-area-message . user-login-name)
  (inhibit-default-init .              t)
  (initial-major-mode .                'fundamental-mode)
  (initial-scratch-message .           nil))

;;;; Simple
;;;;

(leaf simple
  :tag "builtin" "defaults"
  :custom
  (idle-update-delay .                   1.2)
  (blink-matching-paren .                nil)
  (track-eol .                           t)
  (completion-show-help .                nil)
  (column-number-mode .                  t)
  (line-number-mode .                    t)
  (kill-do-not-save-duplicates .         t)
  (save-interprogram-paste-before-kill . t)
  (delete-trailing-lines .               nil)
  (set-mark-command-repeat-pop .         t)
  (kill-ring-max .                       30000))

;;;; Advice
;;;;

(leaf advice
  :tag "builtin" "defaults"
  :custom
  ;; silence redefined function warnings
  (ad-redefinition-action . 'accept))

;;;; Lisp subroutines
;;;;

(leaf subr
  :tag "builtin" "defaults"
  :init
  (fset #'yes-or-no-p #'y-or-n-p))

;;;; Windows to the Walls
;;;;

(leaf *windows-to-the-walls
  :doc "For the window, frame, and widgets."
  :config
  (leaf window
    :tag "builtin" "defaults"
    :leaf-defer nil
    :init
    (add-to-list 'display-buffer-alist '("^\\*Warnings\\*$" (display-buffer-no-window)
                                         (allow-no-window . t)))
    (add-to-list 'display-buffer-alist '("^\\*Flycheck errors\\*$" (display-buffer-no-window)
                                         (allow-no-window . t)))
    :custom
    (pop-up-windows . nil)
    ;; favor horizontal splits
    (split-width-threshold . 160))

  (leaf frame
    :tag "builtin" "defaults"
    :leaf-defer nil
    :hook (emacs-startup-hook . window-divider-mode)
    :bind
    ("C-z" . nil)
    :init
    (blink-cursor-mode -1)
    :custom
    (window-divider-default-places .       t)
    (window-divider-default-bottom-width . 1)
    (window-divider-default-right-width .  1))

  (leaf wid-edit
    :tag "builtin" "defaults"
    :custom
    (widget-image-enable . nil)))

;;;; Shmoving the mouse and cursor
;;;;

(leaf *shmoving
  :doc "For the cursor, mouse, and mousewheel."
  :config
  (leaf cursor-sensor
    :tag "builtin" "defaults"
    :hook (minibuffer-setup-hook . cursor-intangible-mode))

  (leaf mouse
    :tag "builtin" "defaults"
    :custom
    ;; middle-click paste at point, not at click
    (mouse-yank-at-point . t))

  (leaf mwsheel
    :tag "builtin" "defaults"
    :custom
    (mouse-wheel-scroll-amount .     '(5 ((shift) . 2)))
    ; don't accelerate scrolling
    (mouse-wheel-progressive-speed . nil)))

;;;; Standard Keybindings and Variables
;;;;

(leaf bindings
  :tag "builtin" "defaults"
  :custom
  ;; disable mode-line mouseovers
  (mode-line-default-help-echo . nil))

;;;; Editing Text
;;;;

(leaf *editing-text
  :doc "For editing text."
  :config
  (leaf select
    :tag "builtin" "defaults"
    :custom
    (x-select-request-type . '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

  (leaf fill
    :tag "builtin" "defaults"
    :custom
    (adaptive-fill-regexp .            "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
    (adaptive-fill-first-line-regexp . "^* *$"))

  (leaf delsel
    :tag "builtin" "defaults"
    :hook (first-input-hook . delete-selection-mode))

  (leaf paragraphs
    :tag "builtin" "defaults"
    :custom
    (sentence-end-double-space . nil))

  (leaf whitespace
    :tag "builtin" "defaults"
    :hook (first-file-hook . whitespace-mode)
    :init
    (put 'whitespace-toggle-options 'disabled t)
    (put 'global-whitespace-toggle-options 'disabled t)))


;;;; Files
;;;;

(leaf files
  :tag "builtin" "defaults"
  :init
  (setq backup-directory-alist (list (cons "." (concat my-cache-dir "backup/"))))
  :custom
  `((large-file-warning-threshold .          15000000)
    (confirm-nonexistent-file-or-buffer .    t)
    (auto-mode-case-fold .                   nil)
    (require-final-newline .                 t)
    (make-backup-files .                     nil)
    
    (auto-save-default .                     nil)
    (find-file-visit-truename .              t) ; resolve symlinks when opening files
    (find-file-suppress-same-file-warnings . t)
    (delete-old-versions .                   t)
    (backup-by-copying .                     t)
    (version-control .                       t)
    (kept-old-versions .                     5)
    (kept-new-versions .                     5)))

;;;; Searching and Replacing

(leaf *search-replace
  :doc "For searching and replacing text"
  :config
  (leaf grep
    :tag "builtin" "search-replace" "defaults"
    :custom
    `((grep-command . ,(executable-find "rg"))
      (find-program . ,(executable-find "fd"))))

  (leaf isearch
    :tag "builtin" "search-replace" "defaults"
    :bind
    (("C-s" . isearch-forward-regexp)
     ("C-M-s" . isearch-forward)
     ("C-r" . isearch-backward-regexp)
     ("C-M-r" . isearch-backward)))

  (leaf replace
    :tag "builtin" "search-replace" "defaults"
    :bind
    (("M-%" . query-replace-regexp)
     ("C-M-%" . query-replace))))

;;;; Security things
;;;;

(leaf *security
  :doc "For keeping ourselves and Emacs secure as we reasonably can."
  :config
  (leaf nsm
    :tag "builtin" "security" "defaults"
    :leaf-defer nil
    :custom
    (network-security-level . 'high))

  (leaf gnutls
    :tag "builtin" "security" "defaults"
    :leaf-defer nil
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
    `((tls-checktrust . gnutls-verify-error)
      (tls-program .    '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                          "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
        --strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                          ;; compatibility fallbacks
                          "gnutls-cli -p %p %h"))))

  (leaf auth-source
    :tag "builtin" "security" "defaults"
    :leaf-defer nil
    :init
    (setq auth-sources
          (list (concat my-etc-dir "authinfo.gpg") "~/.authinfo.gpg"))))

;;;; Misc.
;;;;

(leaf *misc
  :doc "Bunch of miscellaneous things I needed/wanted to configure and group together."
  :config
  (leaf uniquify
    :tag "builtin" "defaults"
    :custom
    (uniquify-buffer-name-style . nil))

  (leaf ansi-color
    :tag "builtin" "defaults"
    :custom
    (ansi-color-for-comint-mode . t))

  (leaf bookmark
    :tag "builtin" "defaults"
    :custom
    `((bookmark-default-file . ,(concat my-etc-dir "bookmarks"))
      (bookmark-save-flag .    t)))

  (leaf url
    :tag "builtin" "defaults"
    :custom
    `((url-configuration-directory . ,(concat my-etc-dir "url/"))
      (url-cache-directory .         ,(concat my-cache-dir "url/"))))

  (leaf tramp
    :tag "builtin" "defaults"
    :custom
    `((tramp-auto-save-directory . ,(concat my-cache-dir "tramp-auto-save/"))
      (tramp-backup-directory-alist . backup-directory-alist)
      (tramp-persistency-file-name . ,(concat my-cache-dir "tramp-persistency.el"))))

  (leaf kmacro
    :tag "builtin" "misc" "defaults"
    :bind
    (("C-x C-k c" . kmacro-call-macro)
     ("C-x C-k S" . kmacro-end-macro)
     ("C-x C-k v" . kmacro-view-macro)))

  (leaf misc
    :tag "builtin" "misc" "default"
    :bind (("M-z" . zap-up-to-char))))

(provide 'defaults)
;;; defaults.el ends here
