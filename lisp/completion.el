;;; completion.el --- Completion in Emacs with Vertico, Consult, Marginalia, and Company with Orderless -*- lexical-binding: t; -*-
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
;;  While I myself am not a minialist, I do appreciate the simplicity yet also featureful-ness of
;;
;;  - Vertico
;;  - Consult
;;  - Marginalia
;;  - and Company
;;
;;  Interesting packages that use the built-in functionalities of Emacs, as opposed to
;;  creating a whole API.
;;
;;; Code:

(require 'lib)

;;; Packages
;;;

;;;; Orderless
;;;;

(leaf orderless
  :ensure t
  :doc "For ordering completions."
  :tag "external" "completion" "complimentary"
  :init
  (setq orderless-matching-styles
        '(orderless-literal orderless-strict-leading-initialism orderless-prefixes)
        completion-styles '(substring flex orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion)))
          (buffer (styles (partial-completion)))
          (info-menu (styles . (partial-completion))))))

;;;; Minibuffer Completion
;;;;

(leaf *minibuffer-completion
  :doc "Completions for finding files, executing commands, etc. The nicest thing about Emacs."
  :config
  (leaf minibuffer
    :tag "builtin" "completion"
    :custom
    ;; IMPORTANT!! Since we use Corfu, we need to set this
    (tab-always-indent . 'complete)
    (completions-format . 'vertical)
    (completion-cycle-threshold . 3)
    (completion-flex-nospace . nil)
    (completion-ignore-case . t)
    (completion-pcm-complete-word-inserts-delimiters . t)
    (read-buffer-completion-ignore-case . t)
    (read-file-name-completion-ignore-case . t)
    (resize-mini-windows . 'grow-only)
    :config
    (unless (package-installed-p 'orderless)
      (setq completion-styles '(basic substring flex partial-completion)))
    (file-name-shadow-mode 1)
    (minibuffer-depth-indicate-mode 1)
    (minibuffer-electric-default-mode 1))

  (leaf vertico
    :ensure t
    :doc "The simplest and my preferred completion system."
    :tag "external" "completion"
    :defun vertico--exhibit
    :bind (:vertico-map
           :package vertico
           ("?" . minibuffer-completion-help)
           ("M-RET" . minibuffer-force-complete-and-exit)
           ("M-TAB" . minibuffer-complete))
    :init
    ;; we have to have this here to start Vertico
    (vertico-mode)

    ;; these can be annoying so we disable the help at startup
    (advice-add #'vertico--setup :after
                (lambda (&rest _)
                  (setq-local completion-auto-help nil
                              completion-show-inline-help nil)))
    :custom
    (vertico-cycle . t))

  (leaf consult
    :ensure t
    :require (consult-vertico consult-org)
    :doc "Consulting the minibuffer for things."
    :tag "external" "completion" "complimentary"
    :bind (([remap repeat-complex-command] . consult-complex-command)
           ("C-c h" . consult-history)
           ([remap recentf-open-files] . consult-recent-file)
           ("C-c m" . consult-mode-command)
           ([remap switch-to-buffer] . consult-buffer)
           ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
           ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
           ("C-x r C-r" . consult-register)
           ([remap bookmark-jump] . consult-bookmark)
           ("C-x C-k C-r" . consult-kmacro)
           ([remap goto-line] . consult-goto-line)
           ("M-g o" . consult-outline)     ;; "M-s o" is a good alternative.
           ("M-g l" . consult-line)        ;; "M-s l" is a good alternative.
           ("M-g m" . consult-mark)        ;; I recommend to bind Consult navigation
           ("M-g k" . consult-global-mark) ;; commands under the "M-g" prefix.
           ("M-g r" . consult-git-grep)    ;; or consult-grep, consult-ripgrep
           ("M-g f" . consult-fdfind)      ;; or consult-fdfind, consult-locate
           ([remap imenu] . consult-imenu)
           ("M-g M-i" . consult-any-imenu)
           ("M-g e" . consult-error)
           ("M-s m" . consult-multi-occur)
           ([remap yank-pop] . consult-yank-pop)
           ([remap apropos] . consult-apropos)
           (:isearch-mode-map
            :package isearch
            ([remap isearch-edit-string] . consult-isearch)))
    :init
    (fset 'multi-occur #'consult-multi-occur)
    (autoload 'projectile-project-root "projectile")
    :custom
    (register-preview-delay . 0)
    (register-preview-function . #'consult-register-preview)
    (consult-preview-key . 'any)
    (consult-project-root-function . #'projectile-project-root))

  (leaf marginalia
    :ensure t
    :doc "Adding useful annotations in the margins of the minibuffer."
    :tag "external" "completion" "complimentary"
    :bind (:minibuffer-local-map
           :package minibuffer
           ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode)
    :custom
    (marginalia-annotators . '(marginalia-annotators-light marginalia-annotators-heavy nil)))

  (leaf embark
    :ensure t
    :tag "external" "completion"
    :bind ("C-S-a" . embark-act)
    :preface
    (defun embark:resize-collect-window ()
      (when (memq embark-collect--kind '(:live :completions))
        (fit-window-to-buffer (get-buffer-window)
                              (floor (frame-height) 2) 1)))
    :config
    (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
    (add-hook 'embark-collect-post-revert-hook #'embark:resize-collect-window))

  (leaf embark-consult
    :ensure t
    :tag "external" "completion" "complimentary"
    :after (embark consult)
    :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode)))

;;;; Dabbrev
;;;;

(leaf dabbrev
  :tag "builtin" "completion"
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;; Company
;;;;

(leaf *company
  :doc "Tab completion in the buffer."
  :config
  (leaf company
    :ensure t
    :tag "external" "completion"
    :diminish
    :global-minor-mode global-company-mode
    :defun (company-dabbrev-ignore-case company-dabbrev-downcase)
    :commands company-complete-common company-manual-begin company-grab-line company-cancel
    :hook (first-input-hook . global-company-mode)
    :preface
    (defvar company--backend-alist
      '((text-mode company-semantic company-dabbrev company-yasnippet company-ispell)
        (prog-mode company-semantic company-capf company-dabbrev-code)
        (conf-mode company-semantic company-capf company-dabbrev-code)))

    (defun comapny:set-backend (modes &rest backends)
      (declare (indent defun))
      (dolist (mode (enlist modes))
        (if (null (car backends))
            (setq company-backend-alist
                  (delq (assq mode company-backend-alist)
                        company-backend-alist))
          (setf (alist-get mode company-backend-alist)
                backends))))

    (defun company:backends ()
      (let (backends)
        (let ((mode major-mode)
              (modes (list major-mode)))
          (while (setq mode (get mode 'derived-mode-parent))
            (push mode modes))
          (dolist (mode modes)
            (dolist (backend (append (cdr (assq mode company--backend-alist))
                                     (default-value 'company-backends)))
              (push backend backends)))
          (delete-dups
           (append (cl-loop for (mode . backends) in company--backend-alist
                            if (or (eq major-mode mode)  ; major modes
                                   (and (boundp mode)
                                        (symbol-value mode))) ; minor modes
                            append backends)
                   (nreverse backends))))))

    (defun company:init-backends ()
      "Set `company-backends' for the current buffer."
      (or (memq major-mode '(fundamental-mode special-mode))
          buffer-read-only
          (buffer-temp-p (or (buffer-base-buffer) (current-buffer)))
          (setq-local company-backends (company:backends))))

    (put 'company-init-backends 'permanent-local-hook t)

    :custom
    `((company-idle-delay . 0.2)
      (company-idle-delay . 0.2)
      (company-echo-delay . ,(if (display-graphic-p) nil 0))
      (company-tooltip-idle-delay . 0.2)
      (company-tooltip-limit . 14)
      (company-tooltip-align-annotations . t)
      (company-require-match . 'never)
      (company-global-modes . '(not erc-mode message-mode help-mode gud-mode))
      (company-frontends . '(company-pseudo-tooltip-frontend
                             company-echo-metadata-frontend))
      (company-backends . '(company-semantic company-capf))
      (company-auto-complete . nil)
      (company-auto-complete-chars . nil)
      (company-dabbrev-other-buffers . nil)
      (company-dabbrev-ignore-case . nil)
      (company-dabbrev-downcase . nil))
    :defer-config
    (when (package-installed-p 'evil)
      (add-hook 'company-mode-hook #'evil-normalize-keymaps)
      (advice-add 'company-begin-backend :before
                  (defun company:abort-previous (&rest _)
                    (company-abort))))
    (eval-after-load 'company-files
      (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))
    (require 'company-tng)
    (add-to-list 'company-frontends 'company-tng-frontend))

  (leaf company-dict
    :tag "external" "completion" "complimentary"
    :ensure t
    :after company))

;;;; Corfu
;;;;

;; (use-package corfu
;;   :straight (corfu :type git :host github
;;                    :repo "minad/corfu"
;;                    :branch "history")
;;   :hook (first-input . corfu-global-mode)
;;   :bind (:map corfu-map
;;          ("TAB" . corfu-next)
;;          ("S-TAB" . corfu-previous))
;;   :config
;;   (require 'corfu-history)
;;   (add-hook 'corfu--mode-hook 'corfu-histroy-mode)
;;   :custom
;;   (corfu-cycle t))

(provide 'completion)
;;; completion.el ends here
