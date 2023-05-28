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
(require 'init-benchmarking) ;; Measure startup time
(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH
(require 'init-utils)
(require 'init-gui-frames)
(require 'init-themes)
(require 'init-git)
(require 'init-projectile)
(require 'init-grep)
(require 'init-eglot)
(require 'init-org)
(require 'init-markdown)
(require 'init-snippets)

;; UTF-8 mode
(set-language-environment "UTF-8")


;; Font Choices
(cond
 ((find-font (font-spec :name "Fira Code"))
  (set-frame-font "Fira Code-15"))

 ;; Fallback Options
  ((find-font (font-spec :name "Consolas"))
   (set-frame-font "Consolas-15"))
  ((find-font (font-spec :name "Monaco"))
   (set-frame-font "Monaco-15"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (set-frame-font "DejaVu Sans Mono-15")))

;; ido-mode
;;(ido-mode 1)
;;(ido-everywhere)
;;(defvar ido-mode-flex-matching t)



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

;; Install use-package
(mapc
 (lambda (package)
   (if (not (package-installed-p package))
       (progn
         (package-refresh-contents)
         (package-install package))))
 '(use-package diminish bind-key))


;; Install packages.
(dolist (package '(paredit rainbow-delimiters))
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Custom command.
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

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

;; Interactively do things.
;; fido-mode
;; `fido-mode' is provided by icomplete.el
(use-package icomplete
  :hook (after-init . fido-mode))

;; which-key
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Doom modeline
(use-package doom-modeline
  :config
  (setq doom-modeline-height 25)
  (set-face-background 'doom-modeline-bar (face-background 'mode-line))
  (setq doom-modeline-bar-width 1)
  (doom-modeline-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-cliplink grab-mac-link yasnippet yaml-mode whole-line-or-region which-key vlf use-package unfill symbol-overlay rg rainbow-delimiters paredit page-break-lines org-roam ns-auto-titlebar multiple-cursors move-dup mode-line-bell markdown-mode magit-todos list-unicode-display ibuffer-projectile highlight-escape-sequences gnu-elpa-keyring-update git-timemachine git-modes git-link git-blamed fullframe exec-path-from-shell disable-mouse diminish default-text-scale dashboard consult-eglot browse-kill-ring beacon avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
