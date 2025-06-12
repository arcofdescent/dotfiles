;; Set up package.el to work with MELPA
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; UTF-8 support
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; font
(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :height 110)

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

;; General Tree-sitter configuration
(require 'treesit)
(setq treesit-extra-load-path '(expand-file-name "~/.config/emacs/tree-sitter/")) ;; Ensure Emacs looks here, though it often does by default

;; Map file extensions to Tree-sitter major modes
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)) ; This is crucial for TSX
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode)) ; This is crucial for JSX

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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

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

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
	      ("M-SPC"      . corfu-insert-separator)
	      ("TAB"        . corfu-next)
	      ([tab]        . corfu-next)
	      ("S-TAB"      . corfu-previous)
	      ([backtab]    . corfu-previous)
	      ("S-<return>" . corfu-insert)
	      ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :bind (:map flycheck-mode-map
;; 	      ("M-n" . flycheck-next-error) ; optional but recommended error navigation
;; 	      ("M-p" . flycheck-previous-error)))

;; (use-package lsp-mode
;;   :diminish "LSP"
;;   :ensure t
;;   :hook ((lsp-mode . lsp-diagnostics-mode)
;; 	 (lsp-mode . lsp-enable-which-key-integration)
;; 	 ((tsx-ts-mode
;; 	   typescript-ts-mode
;; 	   js-ts-mode) . lsp-deferred))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
;;   (lsp-completion-provider :none)       ; Using Corfu as the provider
;;   (lsp-diagnostics-provider :flycheck)
;;   (lsp-session-file (locate-user-emacs-file ".lsp-session"))
;;   (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
;;   (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
;;   (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
;;   ;; core
;;   (lsp-enable-xref t)                   ; Use xref to find references
;;   (lsp-auto-configure t)                ; Used to decide between current active servers
;;   (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
;;   (lsp-enable-dap-auto-configure t)     ; Debug support
;;   (lsp-enable-file-watchers nil)
;;   (lsp-enable-folding nil)              ; I disable folding since I use origami
;;   (lsp-enable-imenu t)
;;   (lsp-enable-indentation nil)          ; I use prettier
;;   (lsp-enable-links nil)                ; No need since we have `browse-url'
;;   (lsp-enable-on-type-formatting nil)   ; Prettier handles this
;;   (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
;;   (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
;;   (lsp-enable-text-document-color nil)   ; This is Treesitter's job

;;   (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
;;   (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
;;   ;; completion
;;   (lsp-completion-enable t)
;;   (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
;;   (lsp-enable-snippet t)                         ; Important to provide full JSX completion
;;   (lsp-completion-show-kind t)                   ; Optional
;;   ;; headerline
;;   (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
;;   (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
;;   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; modeline
;;   (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
;;   (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
;;   (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
;;   (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
;;   (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
;;   (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
;;   ;; lens
;;   (lsp-lens-enable nil)                 ; Optional, I don't need it
;;   ;; semantic
;;   (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

;;   :init
;;   (setq lsp-use-plists t))

;; (use-package lsp-completion
;;   :no-require
;;   :hook ((lsp-mode . lsp-completion-mode)))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands
;;   (lsp-ui-doc-show
;;    lsp-ui-doc-glance)
;;   :bind (:map lsp-mode-map
;;               ("C-c C-d" . 'lsp-ui-doc-glance))
;;   :after (lsp-mode evil)
;;   :config (setq lsp-ui-doc-enable t
;;                 evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
;;                 lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;                 lsp-ui-doc-include-signature t       ; Show signature
;;                 lsp-ui-doc-position 'at-point))

(use-package treemacs
  :custom
  (treemacs--icon-size 16)
  :bind ("C-c t" . treemacs-select-window))
(use-package treemacs-evil)

(global-set-key (kbd "C-x c") 'quick-calc)

(use-package treesit-auto
  :defer t
  :custom
  ;; 'prompt will ask before installing. 't will install automatically.
  (treesit-auto-install 'prompt)
  :config
  ;; Add specific language sources if treesit-auto doesn't have them built-in,
  ;; though it usually does for common ones like typescript.
  ;; (add-to-list 'treesit-auto-lang-recipe-alist '(typescript (:url "https://github.com/tree-sitter/tree-sitter-typescript.git" :source-dir "typescript/src" :library-name "typescript")))
  ;; (add-to-list 'treesit-auto-lang-recipe-alist '(jsx (:url "https://github.com/tree-sitter/tree-sitter-javascript.git" :source-dir "jsx/src" :library-name "jsx")))
  
  ;; This tells treesit-auto to handle all languages it knows about
  (treesit-auto-add-to-auto-mode-alist 'all) 
  (global-treesit-auto-mode))

(use-package dockerfile-mode)

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode)

(use-package quelpa)
(use-package quelpa-use-package)

(use-package s)
(use-package dash)
(use-package editorconfig)
(use-package company)

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

(setq warning-minimum-level :error)
