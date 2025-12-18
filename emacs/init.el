;; -*- lexical-binding: t -*-

;; ####################################################################
;; # 1. PACKAGE MANAGEMENT (using the built-in `package.el`)
;; ####################################################################
;; This section ensures `package.el` is ready and sets up the MELPA
;; archive, which has the largest collection of Emacs packages.

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


(package-initialize)

;; use-package is built-in to Emacs 30+
(require 'use-package)
(setq use-package-always-ensure t)


;; ####################################################################
;; # 2. MACOS SPECIFIC TWEAKS
;; ####################################################################
;; This is the most important section for a smooth experience on macOS.

;; Use Command key (⌘) as Super, and Option key (⌥) as Meta.
;; This feels natural on a Mac keyboard.
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)


;; ####################################################################
;; # 3. UI & UX CONFIGURATION
;; ####################################################################

;; A. Basic UI Cleanup
;; -------------------
(tool-bar-mode -1)      ; Disable the toolbar
(menu-bar-mode t)      ; Disable the menu bar
(scroll-bar-mode -1)    ; Disable scroll bars
(setq inhibit-startup-screen t) ; Disable the splash screen

;; B. Font and Theme
;; -----------------
;; Set SF Mono as the default font (Apple's excellent coding font)
(set-face-attribute 'default nil :font "SF Mono" :height 160)

;; Enable programming ligatures (e.g., combines '!=' into '≠')
(setq-default prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode 1)


;; Use doom-one theme - a popular dark theme from doom-emacs
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Install all-the-icons for proper icon support
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Use doom-modeline for a more attractive status line
;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1)
;;  :config
;;  (setq doom-modeline-height 28
;;        doom-modeline-bar-width 4
;;        doom-modeline-unicode-fallback t          ; Use fallback for unicode issues
;;        doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Make modeline more readable by customizing faces
;;  (custom-set-faces
;;;;;;;;   '(mode-line ((t (:background "#3f444a" :foreground "#bbc2cf" :box (:line-width 1 :color "#5B6268")))))
;;   '(mode-line-inactive ((t (:background "#23272e" :foreground "#5B6268"))))
   ;; Fix line number colors to match doom-one theme
;;   '(line-number ((t (:foreground "#5B6268" :background "#3f444a"))))
;;   '(line-number-current-line ((t (:foreground "#51afef" :background "#3f444a" :weight bold))))
   ;; Add divider between line numbers and editor
;;   '(fringe ((t (:background "#21242b" :foreground "#5B6268"))))))

;; C. Quality of Life
;; ------------------
(global-display-line-numbers-mode t) ; Show line numbers
(delete-selection-mode t)            ; Typing replaces the selection
(electric-pair-mode t)               ; Auto-close parentheses, quotes, etc.
(column-number-mode t)               ; Show column number in the modeline
(global-auto-revert-mode t)          ; Auto-refresh buffers when files change on disk
(fset 'yes-or-no-p 'y-or-n-p)         ; Use y/n instead of "yes" or "no"

;; Open existing files as read-only by default
(add-hook 'find-file-hook
          (lambda ()
            (when (file-exists-p (buffer-file-name))
              (read-only-mode 1))))


;; ####################################################################
;; # 4. ESSENTIAL PACKAGES
;; ####################################################################

;; `which-key`: Shows available keybindings when you pause after a prefix key.
;; Invaluable for learning and discovery.
(use-package which-key
  :config
  (which-key-mode))

;; `vertico`: A modern, minimal, and fast vertical completion UI.
;; This is a huge improvement over the default completion system.
(use-package vertico
  :init
  (vertico-mode)
  ;; Add some tweaks for a better experience.
  (setq vertico-cycle t))

;; `marginalia`: Adds helpful annotations to completion candidates in the minibuffer.
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; `orderless`: A more powerful and intuitive completion style.
;; Allows you to type space-separated terms in any order.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Make saving the file list on exit less annoying
(setq save-place-file (concat user-emacs-directory "places"))

;; some more plugins
(use-package magit
  :bind ("C-x g" . magit-status))


;; ediff config
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; A suite of powerful search and navigation commands
(use-package consult
  ;; Bind keys globally. The syntax is a simple list of ("key" . command).
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c g" . consult-git-grep)
         ("C-c r" . consult-ripgrep)))

;; Set the file where customizations are saved
(setq custom-file (concat user-emacs-directory "custom.el"))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; TypeScript and JavaScript syntax support
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning nil))

;; Additional JavaScript/TypeScript enhancements
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; JSON support
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonc\\'" . json-mode)))



;; ####################################################################
;; # 5. SERVER MODE CONFIGURATION
;; ####################################################################
;; Start Emacs server for fast client connections
(require 'server)
(unless (server-running-p)
  (server-start))

;; Prevent accidentally killing Emacs server with C-x C-c
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c") (lambda ()
                                  (interactive)
                                  (if (y-or-n-p "Really exit Emacs server? ")
                                      (save-buffers-kill-emacs)
                                    (message "Server exit canceled"))))

;; Alternative way to safely kill Emacs
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

;; Save autosaved file to temp folder.
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
