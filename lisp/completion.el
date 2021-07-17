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

(use-package orderless
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

(use-package minibuffer
  :custom
  ;; IMPORTANT!! Since we use Corfu, we need to set this
  (tab-always-indent 'complete)
  (completions-format 'vertical)
  (completion-cycle-threshold 3)
  (completion-flex-nospace nil)
  (completion-ignore-case t)
  (completion-pcm-complete-word-inserts-delimiters t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (resize-mini-windows 'grow-only)
  :config
  (unless (package-installed-p 'orderless)
    (setq completion-styles '(basic substring flex partial-completion)))
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package vertico
  :bind (:vertico-map
         :package vertico
         ("?" . minibuffer-completion-help)
         ("M-RET" . minibuffer-force-complete-and-exit)
         ("M-TAB" . minibuffer-complete))
  :functions vertico--exhibit
  :init
  ;; we have to have this here to start Vertico
  (vertico-mode)

  ;; these can be annoying so we disable the help at startup
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil)))
  :custom
  (vertico-cycle t))

(use-package consult
  :general
  ;; Remappings
  ([remap recentf-open-files]            #'consult-recent-file)
  ([remap switch-to-buffer]              #'consult-buffer)
  ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  ([remap bookmark-jump]                 #'consult-bookmark)
  ([remap yank]                          #'consult-yank-from-kill-ring)
  ([remap yank-pop]                      #'consult-yank-pop)
  ([remap locate]                        #'consult-locate)
  ([remap repeat-complex-command]        #'consult-complex-command)
  ;; Editing
  ("C-x C-k C-r" #'consult-kmacro)
  ;; Register
  ("C-x r C-r" #'consult-register)
  ("C-x r C-s" #'consult-register-store)
  ;; Navigation
  ("M-g e"   #'consult-error)
  ("M-g M-e" #'consult-compile-error)
  ("M-g f"   #'consult-flymake)
  ("M-g g"   #'consult-goto-line)
  ("M-g o"   #'consult-outline)
  ("M-g m"   #'consult-mark)
  ("M-g k"   #'consult-global-mark)
  ("M-g i"   #'consult-imenu)
  ("M-g I"   #'consult-project-imenu)
  ;; Searching
  ("M-s L" #'consult-line)
  ("M-s k" #'consult-keep-lines)
  ("M-s u" #'consult-focus-lines)
  ("M-s m" #'consult-multi-occur)
  ;; Grep and Find
  ("M-s g" #'consult-ripgrep)
  ("M-s f" #'consult-fdfind)
  ;; Histories
  ("C-c h" #'consult-history)
  ;; Modes
  ("M-SPC m m"   #'consult-minor-mode-menu)
  ("M-SPC m M-m" #'consult-mode-command)
  ;; Isearch
  ("M-s e"                      #'consult-isearch)
  (:keymaps 'isearch-mode-map
   ([remap isearch-edit-string] #'consult-isearch)
   ("M-s l"                     #'consult-line))
  ;; Misc.
  ([remap apropos] #'consult-apropos)
  :init
  (fset 'multi-occur #'consult-multi-occur)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :custom
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview)
  (consult-preview-key 'any)
  (consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :bind (:minibuffer-local-map
         :package minibuffer
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-light marginalia-annotators-heavy nil)))

(use-package embark
  :bind (("M-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map vertico-map
         ("M-e" . embark-export)
         ("M-c" . embark-collect-snapshot))
  :preface
  (defun embark:resize-collect-window ()
    (when (memq embark-collect--kind '(:live :completions))
      (fit-window-to-buffer (get-buffer-window)
                            (floor (frame-height) 2) 1)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
  (add-hook 'embark-collect-post-revert-hook #'embark:resize-collect-window))

(use-package embark-consult
  :after (:all embark consult)
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

;;;; Dabbrev
;;;;

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;; Company
;;;;

;;;###autoload
(defvar company:backend-alist
  '((text-mode company-dabbrev  company-ispell)
    (prog-mode company-capf company-dabbrev-code)
    (conf-mode company-capf company-dabbrev-code)))

;;;###autoload
(defun comapny:set-backend (modes &rest backends)
  (declare (indent defun))
  (dolist (mode (enlist modes))
    (if (null (car backends))
        (setq company:backend-alist
              (delq (assq mode company:backend-alist)
                    company:backend-alist))
      (setf (alist-get mode company:backend-alist)
            backends))))

;;;###autoload
(defun company:backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode company:backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in company:backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

;;;###autoload
(defun company:init-backends ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (buffer-temp-p (or (buffer-base-buffer) (current-buffer)))
      (setq-local company-backends (company:backends))))

(use-package company
  :diminish
  :functions (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line
             company-cancel)
  :hook (first-input-hook . global-company-mode)
  :preface
  (put 'company-init-backends 'permanent-local-hook t)
  :init
  (add-hook 'global-company-mode-hook #'company-tng-mode)
  (add-to-list 'company-frontends 'company-tng-frontend)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2)
  (company-idle-delay 0.2)
  (company-echo-delay (if (display-graphic-p) nil 0))
  (company-tooltip-idle-delay 0.2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode))
  (company-frontends '(company-pseudo-tooltip-frontend
                         company-echo-metadata-frontend))
  (company-backends '(company-capf))
  (company-auto-complete nil)
  (company-auto-complete-chars nil)
  (company-auto-comit nil)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :config
  (when (package-installed-p 'evil)
    (add-hook 'company-mode-hook #'evil-normalize-keymaps)
    (advice-add 'company-begin-backend :before
                (defun company:abort-previous (&rest _)
                  (company-abort))))

  (add-hook 'after-change-major-mode-hook #'company:init-backends 'append)

  (eval-after-load 'company-files
    (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)")

  (eval-after-load 'eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))))

(use-package company-dict
  :defer t
  :after company)

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
