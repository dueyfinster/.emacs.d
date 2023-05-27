;;; my-org.el -*- lexical-binding: t; -*-

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

(use-package gnuplot)

;; Org babel
(org-babel-do-load-languages
'org-babel-load-languages
'((C . t)
  (css . t)
  (dot . t)
  (ditaa . t)
  (elixir . t)
  (emacs-lisp . t)
  (gnuplot . t)
  (java . t)
  (js . t)
  (python . t)
  (ruby . t)
  (shell . t)))

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/notes"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(("d" "default" plain "%?"
                                 :target (file+head "${slug}.org"
                                                    "#+title: ${title}\n")
                                 :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-setup))

(setq org-roam-dailies-directory "journal/")


;;; Package:
(provide 'my-org)
;;; my-org.el ends here