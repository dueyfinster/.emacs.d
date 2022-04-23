
;; Customize user interface.
(menu-bar-mode 1)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

;; Dark theme.
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")

;; ido-mode
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)

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

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

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

;; Custom key sequences.
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

;; Start server.
(require 'server)
(unless (server-running-p)
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

;; Org mode
(use-package org
    :requires htmlize
    :ensure t
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

;; Org Agenda
(setq org-agenda-files (list org-directory))
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

;; Org Tags
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

;; Org Capture
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
	("L" "Protocol Link" entry (file+headline ,(concat org-directory "inbox.org") "Tasks")
        "* %? [[%:link][%:description]] \nCaptured On: %U")
    ))

;; Org task states
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "PROJ(p)" "|" "DONE(d)")
              (sequence "DELEGATED(e@/!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "DeepSkyBlue2" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("DELEGATED" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PROJ" :foreground "cornflower blue" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Org babel
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

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-directory (expand-file-name "~/notes/notes"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))


;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
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
