(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(electric-pair-mode t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(neo-confirm-create-file 'off-p)
 '(org-agenda-files '("~/Dropbox/notes" "/z/apps/argus/notes.org"))
 '(org-default-notes-file "~/Dropbox/notes/notes.org")
 '(org-directory "~/Dropbox/notes")
 '(package-selected-packages
   '(docker-compose-mode docker yaml-mode julia-mode typescript-mode markdown-mode go-mode all-the-icons company-mode highlight-indent-guides ivy doom-modeline use-package anki-editor htmlize haskell-mode magit evil-commentary neotree alchemist zenburn-theme evil))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; packages
(use-package anki-editor
  :ensure t)
(use-package htmlize
  :ensure t)
(use-package haskell-mode
  :ensure t)
(use-package magit
  :ensure t)
(use-package evil-commentary
  :ensure t)
(use-package neotree
  :ensure t)
(use-package alchemist
  :ensure t)
(use-package zenburn-theme
  :ensure t)
(use-package evil
  :ensure t)
(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package ivy
  :ensure t
  :config (ivy-mode 1))
(use-package idomenu
  :ensure t
  :bind ("C-c i" . idomenu))
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'bitmap))
(use-package company
  :config (global-company-mode 1)
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package rjsx-mode
  :ensure t)
(use-package typescript-mode
  :mode (("\\.tsx\\'" . typescript-mode))
  :ensure t)
(use-package web-mode
  :ensure t)
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(use-package julia-mode
  :ensure t)

;; highligh active line
(global-hl-line-mode 1)

(global-flycheck-mode)

;; no auto save
(setq auto-save-default nil)

;; desktop
;; Don't save frame and window configuration
(setq desktop-restore-frames nil)

;; backups
(setq backup-directory-alist '(("." . "~/backups/emacs")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(evil-mode 1)
(load-theme 'zenburn t)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key [f8] 'neotree-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)

(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; don't show organice bak files
(setq neo-hidden-regexp-list '("^\\." "\\.organice-bak$" "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "__pycache__"))

(setq neo-window-fixed-size nil)
(setq neo-window-width 32)

;; gc -> comment/uncomment region
;; gcc -> comment/uncomment line
(evil-commentary-mode)

;; auto insert closing paren
(electric-pair-mode)

;; show matching paren
(show-paren-mode 1)

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO" "PROJ" "IN_PROGRESS" "|" "DONE")))

(setq org-startup-indented t)
(setq org-startup-folded t)

;; Run/highlight code using babel in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   ;; Include other languages here...
   ))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; prev/next buffer
(global-set-key (kbd "C-x C-]") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)

;; vue files, set html/js mode
;; (global-set-key (kbd "C-x v j") 'js-mode)
;; (global-set-key (kbd "C-x v h") 'html-mode)

;; emacs copying clipboard fix
(setq x-selection-timeout 10)

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; no pesky lock files
(setq create-lockfiles nil)

;; TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; TSX
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(setq-default typescript-indent-level 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-indent-style 2)
(setq web-mode-markup-indent-offset 2)
(setq-default indent-tabs-mode nil)


(add-hook 'go-mode-hook
          (lambda ()
            (setq-default)
            (setq tab-width 4)
            (setq standard-indent 4)
            (setq indent-tabs-mode nil)))

(let ((govet (flycheck-checker-get 'go-vet 'command)))
  (when (equal (cadr govet) "tool")
    (setf (cdr govet) (cddr govet))))
