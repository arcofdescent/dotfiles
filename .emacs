;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ivy
(ivy-mode)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; no auto save
(setq auto-save-default nil)

;; no pesky lock files
(setq create-lockfiles nil)

;; org clean view
(setq org-startup-indented t)
(setq org-startup-folded t)

;; crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)
