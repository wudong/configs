;;; init.el --- Emacs configuration optimized for macOS

;; Basic settings
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; UTF-8 encoding for proper Unicode support
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; macOS settings
(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil)
  (setq select-enable-clipboard t))

;; UI improvements
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; Font configuration (using JetBrains Mono for excellent Unicode support)
(when (find-font (font-spec :name "JetBrains Mono Medium"))
  (set-face-attribute 'default nil
                      :font "JetBrains Mono Medium"
                      :height 160))

;; Fallback to Monaco if JetBrains Mono is not available
(when (not (find-font (font-spec :name "JetBrains Mono Medium")))
  (set-face-attribute 'default nil
                      :font "Monaco"
                      :height 160
                      :weight 'normal))

;; Font fallback for better Unicode support
(set-fontset-font t nil "Symbola" nil 'append)
(set-fontset-font t nil "Apple Color Emoji" nil 'append)

;; Theme (matching iTerm2 dark theme)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; All-the-icons for doom-modeline
(use-package all-the-icons
  :ensure t)

;; Modeline (with full Unicode support for SF Mono)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e t)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  ;; Enable all icons (SF Mono supports Unicode)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t))

;; Company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Ivy
(use-package ivy
  :ensure t
  :diminish
  :config (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; Emacs server configuration
(require 'server)

;; Start server if not running, otherwise connect to existing server
(unless (server-running-p)
  (server-start))

;; Ensure server starts when Emacs is started with a file
(defun ensure-server-start ()
  "Start Emacs server if not already running."
  (unless (server-running-p)
    (server-start)))

;; Hook to ensure server is started
(add-hook 'after-init-hook 'ensure-server-start)

;;; init.el ends here
