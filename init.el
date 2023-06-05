;; Set up package.el to work with MELPA
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Download Evil
(unless (package-installed-p 'evil)
(package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; UTF-8 support
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; theme
(load-theme 'tango-dark)

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; Turn off some unneeded UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Download which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))

;; Enable which-key
(require 'which-key)
(which-key-mode)

;; Download evil-commentary
(unless (package-installed-p 'evil-commentary)
  (package-install 'evil-commentary))

;; Enable evil-commentary
(evil-commentary-mode)
