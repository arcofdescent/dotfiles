(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(package-selected-packages
   '(org ivy magit slime zenburn-theme evil-commentary neotree evil))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq evil-want-C-i-jump nil)

;; evil mode
(evil-mode 1)

;; theme
(load-theme 'zenburn t)

;; auto insert closing paren
;; (electric-pair-mode)

;; slime
(setq inferior-lisp-program "sbcl")

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ivy
(ivy-mode)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; neotree
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-fixed-size nil)
(setq neo-window-width 32)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; gc -> comment/uncomment region
;; gcc -> comment/uncomment line
(evil-commentary-mode)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; highligh active line
(global-hl-line-mode 1)

;; no auto save
(setq auto-save-default nil)

;; no pesky lock files
(setq create-lockfiles nil)

;; org clean view
(setq org-startup-indented t)
(setq org-startup-folded t)
