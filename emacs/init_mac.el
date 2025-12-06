;; Set up package.el to work with MELPA
;; Optional: If you want to see what Emacs is doing with proxy settings

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Add this to your init.el for testing purposes
(setq gnutls-verify-error nil)
(setq tls-checktrust nil)

;; --- Proxy Configuration to bypass GnuTLS issues ---
; (setq url-proxy-services
;       '(("no_proxy" . "^\\(localhost\\|10\\.\\|127\\.0\\.0\\.1\\|192\\.168\\.\\)") ; Your local IP ranges
;         ("http"  . "http://139.59.47.178:3128")
;         ("https" . "http://139.59.47.178:3128")))
; (setq url-debug t)

(setq gnutls-log-level 4)
(setq gnutls-default-trust-file "/etc/ssl/certs/ca-certificates.crt")

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :ensure t ; Ensures the package is installed
  :init
  ;; Initialize it to grab PATH and other variables from your shell
  (exec-path-from-shell-initialize))

;; UTF-8 support
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; font
;(set-face-attribute 'default nil
;                    :font "DejaVu Sans Mono"
;                    :height 90)

;; theme
(use-package doom-themes
  :init (load-theme 'doom-one t))

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; Turn off some unneeded UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)

(when (display-graphic-p)
  (scroll-bar-mode -1))

(column-number-mode)
(global-hl-line-mode +1)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Show matching parens
(show-paren-mode 1)

;; word wrap
(global-visual-line-mode 1)

;; lazy count for search
(setq isearch-lazy-count t)

;; windmove
(windmove-default-keybindings)

;; no auto save
(setq auto-save-default nil)

;; no pesky lock files
(setq create-lockfiles nil)

;; save place in files
(save-place-mode 1)

;; set warning level
(setq warning-minimum-level :error)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "HOLD(h)" "|" "DONE(d)")))

  (setq org-todo-keyword-faces
        '(("TODO" . "light blue") ("PROG" . "yellow")))

  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(setq org-agenda-files '("~/Dropbox/notes"
                         "~/Dropbox/work/booknow/README.org"))
;; Ibuffer
(global-set-key (kbd "C-x a") 'org-agenda)

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (perl . t)
   (js . t)))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package which-key
  :config
  (which-key-mode))

;; gcc Comment a line
;; gc Comment a visual block
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Vertico for minibuffer completion
(use-package vertico
  :init
  (vertico-mode))

;; Marginalia for annotations in completion
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Orderless for fuzzy matching
(use-package orderless
  :init
  ;; You can customize completion styles per category
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (partial-completion basic))))))

;; Company-mode for in-buffer code completion
(use-package company
  :init
  (global-company-mode)
  :config
  ;; Adjust settings as desired
  (setq company-idle-delay 0.1) ; Shorter delay for popup
  (setq company-minimum-prefix-length 2) ; Start completion after 2 characters
  (setq company-show-numbers t) ; Show numbers for quick selection
  (setq company-tooltip-limit 10)
  ;; Explicitly add company-lsp to the beginning of the backends
  ;; This ensures LSP completions are prioritized.
  ; (add-to-list 'company-backends 'company-lsp)
  )

(use-package treemacs
  :custom
  (treemacs--icon-size 16)
  :bind ("C-c t" . treemacs-select-window))
(use-package treemacs-evil)

(global-set-key (kbd "C-x c") 'quick-calc)

(use-package dockerfile-mode)

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode)

(use-package s
  :ensure t)
(use-package dash
  :ensure t)
(use-package quelpa)
(use-package quelpa-use-package)

;; Tree-sitter core configuration
(use-package tree-sitter
  :ensure t
  :config
  ;; Enable tree-sitter-mode globally if you want it to activate for all supported modes
  ;; However, it's often better to enable it on a per-mode basis as shown below
  ;; (global-tree-sitter-mode)
    
  ;; Map file extensions to Tree-sitter major modes
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)) ; This is crucial for TSX
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode)) ; This is crucial for JSX
  )

;; --- LSP Configuration ---
; (use-package lsp-mode
;   :commands lsp
;   :hook (
;          ;; General JS/TS hooks
;          (js-ts-mode . lsp-deferred)
;          (typescript-ts-mode . lsp-deferred)
;          (tsx-ts-mode . lsp-deferred)
;          ;; IMPORTANT: For .jsx files using js-ts-mode
;          (js-ts-mode . (lambda ()
;                          (when (string-match-p "\\.jsx\\'" (buffer-file-name))
;                            (lsp-deferred))))
;          )
;   :init
;   ;; Set prefix for lsp-command-keymap (e.g., "C-c l")
;   (setq lsp-keymap-prefix "C-c l")
;   :config
;   (setq lsp-enable-which-key-integration t) ;; Integrates with your which-key setup
;   (setq lsp-ui-doc-position 'at-point) ;; Optional: Where documentation pops up
;   (setq lsp-completion-provider :company) ;; Ensure company-mode is used for completion
;
;    ;; **ADD THESE LINES:**
;   (add-to-list 'lsp-language-id-configuration '(javascript-ts-mode . "javascript"))
;   (add-to-list 'lsp-language-id-configuration '(js-ts-mode . "javascript"))
;   (add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "typescript"))
;   (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescript")) ; TSX files also use 'typescript' language ID
;
;   ;; Make sure 'typescript-language-server' is mapped to 'javascript' and 'typescript' language IDs
;   ;; This might be redundant if the above explicit configuration works, but good to have.
;   (add-to-list 'lsp-language-id-configuration '(javascript . ("typescript-language-server")))
;   (add-to-list 'lsp-language-id-configuration '(typescript . ("typescript-language-server")))
;
;
;   ;; This tells LSP where to find the server executable itself.
;   ;; You should still ensure the directory is in `exec-path` or specify the full path here.
;   (setq lsp-typescript-server-path (executable-find "typescript-language-server"))
;
;   ;; If you want to enable logging for the TS server for debugging
;   ;; (setq lsp-typescript-tsserver-log "verbose")
;   )
;
; ;; Optional: for enhanced UI elements like sidebars, peek definitions, etc.
; (use-package lsp-ui
;   :commands lsp-ui-mode
;   :hook (lsp-mode . lsp-ui-mode)
;   :config
;   ;; Adjust lsp-ui settings as desired
;   (setq lsp-ui-sideline-show-hover t)
;   (setq lsp-ui-sideline-show-diagnostics t)
;   (setq lsp-ui-pop-up-show-code-actions t)
;   )
;
;; Optional: for linting/diagnostics (LSP will feed into this)
; (use-package flycheck
;   :init (global-flycheck-mode))

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :init
  (add-hook 'prog-mode-hook #'copilot-mode))
;; you can utilize :map :hook and :config to customize copilot

(defun ra/no-copilot-mode ()
  "Helper for `ra/no-copilot-modes'."
  (copilot-mode -1))

(defvar ra/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun ra/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or ra/copilot-manual-mode
      (member major-mode ra/no-copilot-modes)
      (company--active-p)))

(add-to-list 'copilot-disable-predicates #'ra/copilot-disable-predicate)

(defvar ra/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun ra/copilot-change-activation ()
  "Switch between three activation modes:
      - automatic: copilot will automatically overlay completions
      - manual: you need to press a key (M-C-<return>) to trigger completions
      - off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode ra/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq ra/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq ra/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "M-C-<escape>") #'ra/copilot-change-activation)

(defun ra/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
    is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)

(defun ra/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
  available. Otherwise will try company, yasnippet or normal
  tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (company-yasnippet-or-completion)
      (indent-for-tab-command)))

(define-key global-map (kbd "C-<tab>") #'ra/copilot-tab)

(defun ra/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                     (setq copilot-disable-predicates (list (lambda () t)))
                     (copilot-clear-overlay)
                     (run-with-idle-timer
                      1.0
                      nil
                      (lambda ()
                        (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'ra/copilot-quit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
