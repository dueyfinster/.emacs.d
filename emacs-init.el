;; Don't edit this file, edit /Users/ngrogan/.emacs.d/emacs-init.org instead ...


  ;; Helper function for changing OS platform keywords to system-type strings
  (defun platform-keyword-to-string (platform-keyword)
    (cond
     ((eq platform-keyword 'windows) "windows-nt")
     ((eq platform-keyword 'cygwin) "cygwin")
     ((eq platform-keyword 'osx) "darwin")
     ((eq platform-keyword 'linux) "gnu/linux")))

  ;; Define a macro that runs an elisp expression only on a particular platform
  (defmacro on-platform-do (&rest platform-expressions)
    `(cond
      ,@(mapcar
         (lambda (platform-expr)
       (let ((keyword (nth 0 platform-expr))
             (expr (nth 1 platform-expr)))
         `(,(if (listp keyword)
           `(or
             ,@(mapcar
                (lambda (kw) `(string-equal system-type ,(platform-keyword-to-string kw)))
                keyword))
            `(string-equal system-type ,(platform-keyword-to-string keyword)))
            ,expr)))
         platform-expressions)))


;; Keep transient cruft out of ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;;(use-package benchmark-init
;;  :ensure t
;;  :config
;;  ;; To disable collection of benchmark data after init is done.
;;  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(server-start)
(on-platform-do (mac
  (setq ns-pop-up-frames nil
        x-select-enable-clIpboard t)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode 1))))'
  (setq tls-checktrust t)
  (setq gnutls-verify-error t)
(mapc
 (lambda (package)
   (if (not (package-installed-p package))
       (progn
         (package-refresh-contents)
         (package-install package))))
 '(use-package diminish bind-key))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-always-ensure t)
 (setq user-full-name "Neil Grogan"
       user-mail-address "neil@grogan.org"
       calendar-latitude 53.42
       calendar-longitude -7.94
       calendar-location-name "Athlone, Ireland")

  ;; Thanks, but no thanks
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room

  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Set up the visible bell
  (setq visible-bell t)

  ;; Emacs Lisp as starting mode
  (setq initial-major-mode 'emacs-lisp-mode)
  
  ;; Empty Scratch Buffer
  (setq initial-scratch-message nil)

  ;; Don't warn following Symlinks
  (setq vc-follow-symlinks t)

  ;; Don't warn for large files (like videos)
  (setq large-file-warning-threshold nil)
  (use-package spacegray-theme :defer t)
  (use-package doom-themes :defer t)
  (use-package solarized-theme :config (load-theme 'solarized-dark t))

  ;; Set the font face based on platform
  (on-platform-do
   ((windows cygwin) (set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 130))
    (osx (set-face-attribute 'default nil :font "Fira Mono" :height 170))
    (linux (set-face-attribute 'default nil :font "Fira Code Retina" :height 220)))

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 200)

  ;; Set the variable pitch face
  ;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 245 :weight 'regular)


  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  erc-mode-hook
                  term-mode-hook
                  eshell-mode-hook
                  vterm-mode-hook
                  neotree-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  (defun edit-config-file ()
    (interactive)
    (find-file (concat config-load-path "emacs-init.org")))
;; store all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; backups in backup dir
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 24
      kept-old-versions 12
      global-auto-revert-mode 1
      version-control t)

(setq create-lockfiles nil)
(auto-save-visited-mode t)
(auto-revert-mode t)
;; Load desktop buffers lazily.
  (setq desktop-lazy-idle-delay 2)
  (setq desktop-lazy-verbose nil)
  (setq desktop-restore-eager 0)

  ;; Always save the desktop.
  (setq desktop-save t)

  ;; Enable desktop.
  (desktop-save-mode t)
(use-package recentf
    :defer 1
    :config (recentf-mode 1)
(setq recentf-max-menu-items 300)
(setq recentf-max-saved-items 300)
(setq recentf-exclude
   '("/elpa/" ;; ignore all files in elpa directory
     "recentf" ;; remove the recentf load file
     ".*?autoloads.el$"
     "treemacs-persist"
     "company-statistics-cache.el" ;; ignore company cache file
     "/intero/" ;; ignore script files generated by intero
     "/journal/" ;; ignore daily journal files
     ".gitignore" ;; ignore `.gitignore' files in projects
     "/tmp/" ;; ignore temporary files
     "NEWS" ;; don't include the NEWS file for recentf
     "bookmarks"  "bmk-bmenu" ;; ignore bookmarks file in .emacs.d
     "loaddefs.el"
     "^/\\(?:ssh\\|su\\|sudo\\)?:" ;; ignore tramp/ssh files
     ))
(setq-default recent-save-file "~/.emacs.d/recentf"))
(use-package pdf-tools
  ;  :if (not (string-equal system-type "windows-nt"))
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query)
  :functions (pdf-tools-disable-cursor pdf-tools-advice-evil-refresh-cursor))
(use-package helm
  :ensure t
  :demand
  :diminish helm-mode
  :after (evil)
  :bind (("C-x C-r" . helm-recentf)
           ("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files)
           ("C-c h" . helm-command-prefix)
           ;;("<tab>" . helm-execute-persistent-action)
           ("C-i" . helm-execute-persistent-action)
           ("C-z" . helm-select-action))

    :config (setq projectile-project-search-path '("~/repos/" "~/.dotfiles/"))
            (setq helm-split-window-inside-p t
                  helm-M-x-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-ff-file-name-history-use-recentf t
                  helm-recentf-fuzzy-match t
                  helm-move-to-line-cycle-in-source t
                  projectile-completion-system 'helm)

            ;;Bindings for evil mode
            (define-key evil-ex-map "b " 'helm-mini)
            (define-key evil-ex-map "e" 'helm-find-files)
            (define-key evil-ex-map "x" 'helm-M-x)
            (define-key evil-ex-map "g" 'helm-projectile-rg)
            (define-key evil-ex-map "f" 'helm-projectile-find-file)

            (set-face-attribute 'helm-selection nil :background "cyan")
            (helm-mode 1)
            (helm-adaptive-mode 1))

(use-package helm-rg
           :after (helm))
(use-package evil
  :ensure t
  :config   (evil-mode 1))
(use-package company
    :diminish company-mode
    :init
  (autoload 'helm-company "helm-company") ; Not necessary if using ELPA package
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-<tab>") 'helm-company)
       (define-key company-active-map (kbd "C-<tab>") 'helm-company)))
    :config
   (setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance)))
(use-package helm-company
     :ensure t
     :init (autoload 'helm-company "helm-company"))

(use-package company-lsp
  :ensure t
  :config
 (push 'company-lsp company-backends)
)
  (use-package company-jedi
    :config (add-to-list 'company-backends 'company-jedi))
(use-package magit
    :defer 5
    :ensure t
    :init (progn
           (bind-key "C-x g" 'magit-status)
           ))
(use-package git-gutter
    :ensure t
    :defer 5
    :init
      (global-git-gutter-mode t)
    :diminish git-gutter-mode
    :config
    (dolist (p '((git-gutter:added    . "#0c0")
                (git-gutter:deleted  . "#c00")
                (git-gutter:modified . "#c0c")))
     (set-face-foreground (car p) (cdr p))
     (set-face-background (car p) (cdr p))))
(use-package neotree
    :defer 5
    :after evil
    :bind ([f8] . neotree-toggle)
    :config (setq neo-smart-open t)
             (define-key evil-normal-state-map (kbd "C-<tab>") 'neotree-toggle))
(use-package org
    :requires htmlize
    :ensure t
    :pin org
    :config
     (add-to-list 'org-modules 'org-habit))
(use-package org-protocol
    :ensure nil)
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Set Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; Set Preferences
(setq org-completion-use-ido nil
      org-startup-truncated nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-startup-with-inline-images t
      org-edit-src-content-indentation 0)

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))

;; Which files open with emacs? Or system default app...
(add-to-list 'org-file-apps '("\\.xls\\'" . default))
(add-to-list 'org-file-apps '("\\.xlsx\\'" . default))
;; Set to the location of your Org files on your local system
;; use iCloud client on Windows
(if (eq system-type 'windows-nt)
  (setq org-directory (expand-file-name "C:/Users/egronei/iCloudDrive/iCloud~com~appsonthemove~beorg/org/"))
  (setq org-directory (expand-file-name "~/org/")))


(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-inbox-path (concat org-directory "inbox.org"))
(setq org-gtd-path (concat org-directory "gtd.org"))
(setq org-tickler-path (concat org-directory "tickler.org"))
(setq org-someday-path (concat org-directory "someday.org"))
(setq org-agenda-files `(,org-inbox-path ,org-gtd-path ,org-tickler-path))
(setq org-refile-targets '((org-gtd-path :maxlevel . 1)
                           (org-tickler-path :level . 2)
                           (org-someday-path :maxlevel . 2)))

(setq org-agenda-custom-commands
  (quote (("d" todo "DELEGATED" nil)
      ("c" todo "DONE|DEFERRED|CANCELLED" nil)
      ("w" todo "WAITING" nil)
      ("W" agenda "" ((org-agenda-ndays 21)))
      ("A" agenda ""
        ((org-agenda-skip-function
          (lambda nil
      (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
        (org-agenda-ndays 1)
        (org-agenda-overriding-header "Today's Priority #A tasks: ")))
      ("u" alltodo ""
        ((org-agenda-skip-function
          (lambda nil
      (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
              (quote regexp) "\n]+>")))
        (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
(setq org-tag-alist '(
  (:startgroup . nil)
    ("home" . ?h)
    ("work" . ?w)
  (:endgroup . nil)
  (:startgroup . nil)
    ("@errands" . ?e)
    ("@house" . ?s)
    ("@now" . ?n)
    ("@online" . ?o)
    ("@phone" . ?p)
    ("@office" . ?f)
  (:endgroup . nil)
))
 (setq org-habit-following-days 30)
 (setq org-habit-show-all-today t)
 (setq org-habit-show-habits-only-for-today nil)
(setq org-capture-templates `(
  ("p" "Personal Task" entry (file+headline ,(concat org-directory "gtd.org") "Personal")
               "* TODO %i%?")
  ("w" "Work Task" entry (file+headline ,(concat org-directory "gtd.org") "Work")
               "* TODO %i%?")
  ("t" "Todo [inbox]" entry (file+headline ,(concat org-directory "inbox.org") ,(format "%s %s" (format-time-string "%Y")(format-time-string "%B"))) 
"* TODO %i%?\n %U")
  ("m" "Meeting" entry (file+headline ,(concat org-directory "gtd.org") "Work")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
  ("P" "Phone call" entry (file ,(concat org-directory "gtd.org") "Work")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
  ("T" "Tickler" entry (file+headline ,(concat org-directory "tickler.org")
               "Tickler") "* %i%? \n %U")

  ;; Org-Protocol entries
	("p" "Protocol" entry (file+headline ,(concat org-directory "inbox.org") "Tasks")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file+function ,(concat org-directory "inbox.org") find-date-tree)
        "* %? [[%:link][%:description]] \nCaptured On: %U")
))

(defun get-year-and-month ()
  (list (format-time-string "%Y") (format-time-string "%B")))

(defun find-date-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            )
        (if (re-search-forward re end t)
            (goto-char (point-at-bol)) 
          (progn
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "DELEGATED(e@/!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "DeepSkyBlue2" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("DELEGATED" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
(org-babel-do-load-languages
'org-babel-load-languages
'((emacs-lisp . t)
  (C . t)
  (css . t)
  (ditaa . t)
  (gnuplot . t)
  (ledger . t)
  (java . t)
  (python . t)
  (ruby . t)
  (shell . t)))
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))
(find-file (concat org-directory "gtd.org"))
    (use-package flycheck
      :ensure t
      :init
      (global-flycheck-mode t))
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))
(use-package helm-projectile
  :bind (("C-c v" . helm-projectile)
         ("C-c C-v" . helm-projectile-rg)
         ("C-c w" . helm-projectile-switch-project)))
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (setq yas-snippet-dirs
           '("~/.emacs.d/snippets"                 ;; local snippets
           ))
          (yas-global-mode 1))
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))
(use-package prodigy
:ensure t
:defer t
:config
(prodigy-define-service
  :name "blog@localhost"
  :command "python2"
  :args '("-m" "SimpleHTTPServer" "8000")
  :cwd "~/repos/org-blog"
  :tags '(file-server)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t))
(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode c-mode c++-mode) . lsp))
(use-package company-lsp :ensure t
  :after (lsp-mode company)
  :defer t
  :config
  (progn
    (setq company-lsp-async t)))
(use-package helm-lsp :ensure t
  :after (lsp-mode helm)
  :defer t)
(use-package lsp-ui
  :after lsp-mode
  :defer t
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit t)
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
(use-package dap-mode
  :ensure t 
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
(use-package lsp-java :ensure t :after lsp-mode
  :hook ((java-mode . lsp-mode)
         (java-mode . (lambda () (add-to-list (make-local-variable 'company-backends) 'company-lsp)))))
(setq lsp-python-executable-cmd "python3")
(setq python-shell-interpreter "python3")
