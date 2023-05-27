;;; init.el -*- lexical-binding: t; -*-

;; Customize user interface.
(menu-bar-mode 1)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

;; UTF-8 mode
(set-language-environment "UTF-8")

;; Dark theme
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")


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

;; Add the lisp folder to the load path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

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

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(mapc
 (lambda (package)
   (if (not (package-installed-p package))
       (progn
         (package-refresh-contents)
         (package-install package))))
 '(use-package diminish bind-key))


;; Install packages.
(dolist (package '(markdown-mode paredit rainbow-delimiters))
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

;; magit
(use-package magit
    :defer 5
    :ensure t
    :init (progn
           (bind-key "C-x g" 'magit-status)
           ))


;; Helm
(use-package helm
  :config
  (setq helm-buffers-fuzzy-matching t)
  (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
  (setq helm-recentf-fuzzy-match t))

(use-package helm-rg)

;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind ("C-c p" . projectile-command-map)
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-completion-system 'helm)
  (setq projectile-per-project-compilation-buffer t)
  (setq projectile-comint-mode t)
  (setq compilation-read-command nil)
  (helm-projectile-on)
  (projectile-mode))

(use-package helm-projectile)

;; Company
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2))

  (global-company-mode)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(setq company-tooltip-align-annotations t)

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config (setq yas-snippet-dirs
           '("~/.emacs.d/snippets"                 ;; local snippets
           ))
  (yas-global-mode 1))

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
