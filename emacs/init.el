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

;; no auto save
(setq auto-save-default nil)

;; no pesky lock files
(setq create-lockfiles nil)

;; save place in files
(save-place-mode 1)

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
                         "/z/apps/synapse/README.org"
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

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t))

;; (use-package company
;;   ;; Navigate in completion minibuffer with `C-n` and `C-p`.
;;   :bind (:map company-active-map
;;               ("C-n" . company-select-next)
;;               ("C-p" . company-select-previous))
;;   :commands company-mode
;;   :init
;;   (add-hook 'prog-mode-hook #'company-mode)
;;   (add-hook 'text-mode-hook #'company-mode))

(use-package treemacs
  :custom
  (treemacs--icon-size 16)
  :bind ("C-c t" . treemacs-select-window))
(use-package treemacs-evil)

(global-set-key (kbd "C-x c") 'quick-calc)

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.svelte\\'"
         "\\.js\\'")
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(setq js-indent-level 2)

(use-package dockerfile-mode)
