;;; completion.el --- Completion in Emacs with Vertico, Consult, Marginalia, and Company with Orderless -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alec Stewart
;;
;; Created: February 20, 2021
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
(require 'thingatpt)

;;; Packages
;;;

;;;; Orderless
;;;;

(use-package orderless
  :init
  (setq orderless-matching-styles
        '(orderless-literal orderless-strict-leading-initialism orderless-prefixes)
        orderless-component-separator "[ &]"
        completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (orderless partial-completion)))
          (buffer (styles (orderless partial-completion)))
          (info-menu (styles . (orderless partial-completion)))))
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;;; Minibuffer Completion
;;;;

(use-package minibuffer
  :ensure nil
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

;;;;; Vertico
;;;;;

;;;###autoload
(defun vertico:crm-select ()
  "Enter candidate in `consult-completing-read-multiple'"
  (interactive)
  (let ((idx vertico--index))
    (unless (get-text-property 0 'consult--crm-selected (nth vertico--index vertico--candidates))
      (setq idx (1+ idx)))
    (run-at-time 0 nil (lambda ()
                         (interactive)
                         (vertico--goto idx) (vertico--exhibit))))
  (vertico-exit))

;;;###autoload
(defun vertico:crm-exit ()
  "Enter candidate in `consult-completing-read-multiple'"
  (interactive)
  (run-at-time 0 nil #'vertico-exit)
  (vertico-exit))

;;;###autoload
(defun vertico:search-symbol-at-point ()
  "Consult the lines that have the symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package vertico
  :functions vertico--exhibit
  :init
  (vertico-mode)
  ;; these can be annoying so we disable the help at startup
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil)))
  :custom
  (vertico-resize nil)
  (vertico-count 16)
  (vertico-cycle t)
  :config  
  ;; (define-key 'vertico-map (kbd "?") #'minibuffer-completion-help)
  ;; (define-key 'vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exist)
  ;; (define-key 'vertico-map (kbd "M-TAB") #'minibuffer-complete)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;;;;; Consult
;;;;;

(use-package consult
  :defer t
  :general
  ;; Remappings
  ([remap recentf-open-files]            #'consult-recent-file)
  ([remap switch-to-buffer]              #'consult-buffer)
  ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  ([remap bookmark-jump]                 #'consult-bookmark)
  ([remap yank-pop]                      #'consult-yank-pop)
  ([remap locate]                        #'consult-locate)
  ([remap repeat-complex-command]        #'consult-complex-command)
  ([remap man]                           #'consult-man)
  ([remap imenu]                         #'consult-imenu)
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
  ("M-s f" #'consult-find)
  ;; Histories
  ("C-c h" #'consult-history)
  ;; Modes
  ;;("M-SPC m m"   #'consult-minor-mode-menu)
  ;;("M-SPC m M-m" #'consult-mode-command)
  ;; Isearch
  (:keymaps 'consult-crm-map
   "TAB"                       #'vertico:crm-select
   "RET"                       #'vertico:crm-exit)
  ("M-s e"                     #'consult-isearch)
  (:keymaps 'isearch-mode-map
   [remap isearch-edit-string] #'consult-isearch
   "M-s l"                     #'consult-line)
  ;; Misc.
  ([remap apropos] #'consult-apropos)
  :preface
  :init
  (defvar consult:find-program (cl-find-if #'executable-find (list "fdfind" "fd")))

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile:get-project-root)

  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  (setq prefix-help-command #'embark-prefix-help-command)

  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview
        consult-preview-key '(:debounce 0.2 any)
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-project-root-function #'projectile:get-project-root
        consult-find-args (concat
                           (format "%s -0 -i -H --color=never --follow --exclude .git --regex" consult:find-program)
                           (if windows-nt-p "--path-separator=/")))
  :config
  (defadvice! consult:recent-file-fix (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))
  
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (list (kbd "C-SPC") (kbd "M-.")))

  (consult-customize
   consult-buffer consult-buffer-other-window consult-buffer-other-frame
   consult--source-buffer consult--source-project-buffer consult--source-hidden-buffer
   :preview-key (list :debounce 0.25 (kbd "<up>") (kbd "<down>") (kbd "C-p") (kbd "C-n")
                      :debounce 0.3 'any))

  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-SPC") (kbd "M-.")
                      :debounce 1 'any)))

(use-package consult-dir
  :after (consult)
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flycheck
  :when (package-installed-p 'flycheck)
  :after (consult flycheck))

;;;;; Marginalia
;;;;;

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (advice-add #'marginalia--project-root :override #'projectile:get-project-root)
  (pushnew! marginalia-command-categories
            '(flycheck-error-list-set-filter . builtin)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))

;;;;; Embark
;;;;;

;;; TODO define some Embark maps here
(use-package embark
  :defer t
  :defines vertico-map
  :bind (("C-;" . embark-act)
         ("C-." . embark-dwim)
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("M-e" . embark-export)
         ("M-c" . embark-collect-snapshot)
         :map vertico-map
         ("C-;" . embark-act)
         ("M-e" . embark-export)
         ("M-c" . embark-collect-snapshot))
  :init
  (defun embark:resize-collect-window ()
    (when (memq embark-collect--kind '(:live :completions))
      (fit-window-to-buffer (get-buffer-window)
                            (floor (frame-height) 2) 1)))

  ;(set-face-attribute 'embark-verbose-indicator-title nil :height 1.0)
  :config
  (setq embark-indicator #'embark-verbose-indicator
        embark-verbose-indicator-display-action
        '(display-buffer-at-bottom (window-height . fit-window-to-buffer)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
  (add-hook 'embark-collect-post-revert-hook #'embark:resize-collect-window))

(use-package embark-consult
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;;; Dabbrev
;;;;

(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;; Company
;;;;

;;;###autoload
(defvar company:backend-alist
  '((text-mode company-dabbrev company-ispell)
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
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line
             company-cancel)
  :functions (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (first-input . global-company-mode)
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
  
  (defadvice! saner-completion-styles-for-company-capf (orig-fn &rest args)
    "Orderless doesn’t exactly make Company’s completion all
 that sane. We simply change the ‘completion-styles’ to the
saner options."
    :around #'company-capf
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fn args)))

  (add-hook 'after-change-major-mode-hook #'company:init-backends 'append)

  (eval-after-load 'eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)))

(use-package company-files
  :defer t
  :after company
  :config
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

(use-package company-dict
  :defer t
  :after company
  :config
  (setq company-dict-dir (expand-file-name "dicts" my-etc-dir)))

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
