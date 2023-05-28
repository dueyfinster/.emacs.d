;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Add the lisp folder to the load path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Setting up
(require 'init-benchmarking) ;; Measure startup time
(require 'init-elpa) ;; package management
(require 'init-exec-path) ;; Set up $PATH

;; Load configs for specific features and modes
(require-package 'diminish)

;; Install use-package
(mapc
 (lambda (package)
   (if (not (package-installed-p package))
       (progn
         (package-refresh-contents)
         (package-install package))))
 '(use-package diminish bind-key))

;; General setup
(require 'init-utils)
(require 'init-gui-frames)
(require 'init-themes)
(require 'init-company)
(require 'init-git)
(require 'init-locales)
(require 'init-projectile)
(require 'init-grep)
(require 'init-eglot)
(require 'init-flymake)
(require 'init-snippets)
(require 'init-paredit)
(require 'init-ido)
(require 'init-sessions)
(require 'init-treesitter)

;; Support for Filetypes & Programming Languages
(require 'init-css)
(require 'init-csv)
(require 'init-elixir)
(require 'init-erlang)
(require 'init-html)
(require 'init-java)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-org)
(require 'init-python)
(require 'init-terraform)
(require 'init-yaml)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Refresh files if changed on disk and auto-save
(auto-save-visited-mode t)
(auto-revert-mode t)

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Set repos dir as default
(setq default-directory "~/")

;; Automatically revert buffers for changed files
(global-auto-revert-mode t)

;; remembering the last place you visited in a file
(save-place-mode 1)

;; Close all dired buffers after opening
(setq dired-kill-when-opening-new-dired-buffer t)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(require 'bind-key)

;; Custom key sequences.
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-[") 'next-buffer)
(global-set-key (kbd "M-]") 'previous-buffer)

;;; change window
(global-set-key [(C-tab)] 'other-window)
(global-set-key [(C-M-tab)] 'other-window)

(require 'server)
;; Start a server if (server-running-p) does not return t (e.g. if it
;; returns nil or :other)
(or (eq (server-running-p) t)
    (server-start))

;; which-key
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
