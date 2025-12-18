;; -*- lexical-binding: t -*-

;; Test basic Emacs configuration
(message "Testing basic configuration...")

;; Test package initialization
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Test use-package
(unless (package-installed-p 'use-package)
  (condition-case err
      (progn
        (package-refresh-contents)
        (package-install 'use-package))
    (error
     (message "Failed to install use-package: %s" err))))

(eval-when-compile
  (require 'use-package))

;; Test a simple use-package
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(message "Basic test completed successfully")