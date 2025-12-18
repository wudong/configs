;; -*- lexical-binding: t -*-

;; ####################################################################
;; # 1. PACKAGE MANAGEMENT (using the built-in `package.el`)
;; ####################################################################
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package`, a macro that simplifies package configuration.
(unless (package-installed-p 'use-package)
  (condition-case err
      (progn
        (package-refresh-contents)
        (package-install 'use-package))
    (error
     (message "Failed to install use-package: %s" err))))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; ####################################################################
;; # 2. MACOS SPECIFIC TWEAKS
;; ####################################################################
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; ####################################################################
;; # 3. UI & UX CONFIGURATION
;; ####################################################################

;; Basic UI Cleanup
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Font and Theme
(condition-case nil
    (set-face-attribute 'default nil :font "SF Mono" :height 160)
  (error
   ;; Fallback to other common monospace fonts if SF Mono is not available
   (let ((fonts '("Monaco" "Menlo" "Consolas" "Courier New")))
     (catch 'found
       (dolist (font fonts)
         (when (member font (font-family-list))
           (set-face-attribute 'default nil :font font :height 160)
           (throw 'found font)))))))

;; Use doom-one theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Quality of Life
(global-display-line-numbers-mode t)
(delete-selection-mode t)
(electric-pair-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; ####################################################################
;; # 4. ESSENTIAL PACKAGES
;; ####################################################################

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; vertico completion
(use-package vertico
  :config
  (when (fboundp 'vertico-mode)
    (vertico-mode)
    (setq vertico-cycle t)))

;; marginalia annotations
(use-package marginalia
  :after vertico
  :config
  (when (fboundp 'marginalia-mode)
    (marginalia-mode)))

;; orderless completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; Built-in project management
(require 'project)

;; LSP convenience keybindings (global)
(global-set-key (kbd "C-c l .") 'xref-find-definitions)
(global-set-key (kbd "C-c l ,") 'xref-go-back)
(global-set-key (kbd "C-c l ;") 'xref-find-references)
(global-set-key (kbd "M-?") 'xref-find-references)

;; Eglot LSP client for intelligent code completion
(use-package eglot
  :config
  ;; TypeScript/JavaScript LSP server configuration
  (setq eglot-workspace-configuration
        '((:typescript .
           ((:preferences . ((:includeInlayParameterNameHints . "all")
                           (:includeInlayParameterNameHintsWhenArgumentMatchesName . t)
                           (:includeInlayFunctionParameterTypeHints . t)
                           (:includeInlayVariableTypeHints . t)
                           (:includeInlayPropertyDeclarationTypeHints . t)
                           (:includeInlayFunctionLikeReturnTypeHints . t)
                           (:includeInlayEnumMemberValueHints . t)))))))

  ;; General eglot settings for better UX
  (setq eglot-sync-timeout 2
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)

  ;; Keybindings for LSP features
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c l h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c l s") 'eglot-signature-eldoc-doc)

  ;; Auto-enable eglot for TypeScript and JavaScript files
  :hook
  ((typescript-mode . eglot-ensure)
   (js2-mode . eglot-ensure)
   (web-mode . eglot-ensure)))

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

;; Additional LSP utilities
(use-package xref
  :config
  ;; Use project.el for xref definitions
  (setq xref-search-program 'ripgrep
        xref-show-definitions-function #'xref-show-definitions-completing))

;; consult
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c g" . consult-git-grep)
         ("C-c r" . consult-ripgrep)
         ("C-c p b" . consult-project-buffer)))

;; ####################################################################
;; # 5. SERVER MODE CONFIGURATION
;; ####################################################################
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

;; Save autosaved file to temp folder
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;;; init.el ends here