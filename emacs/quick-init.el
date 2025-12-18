;; -*- lexical-binding: t -*-
;; Minimal Emacs configuration for quick markdown editing

;; Basic UI cleanup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Better editing experience
(setq-default truncate-lines nil)           ; Enable line wrapping
(setq make-backup-files nil)                ; Disable backup files
(setq auto-save-default nil)                ; Disable auto-save files
(setq ring-bell-function 'ignore)           ; Disable bell
(setq scroll-margin 3)                      ; Keep 3 lines above/below cursor
(setq scroll-conservatively 100)            ; Smooth scrolling
(global-display-line-numbers-mode 1)        ; Show line numbers
(column-number-mode 1)                      ; Show column number in mode line


;; Save all buffers and quit without prompting
(defun save-all-and-quit ()
  "Save all file-visiting buffers and quit Emacs."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'save-all-and-quit)

;; Move cursor to end of buffer when opening files
(add-hook 'find-file-hook 'end-of-buffer)

;; Add padding around editor
(set-window-fringes nil 8 8)
(setq-default left-margin-width 2)
(setq-default right-margin-width 2)
