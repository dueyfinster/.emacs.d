;;; init-org.el -- Org Mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

(maybe-require-package 'org-cliplink)

(setq
 org-default-notes-file "~/org/gtd.org"
 initial-buffer-choice  org-default-notes-file)


;; Set Keybindings
(global-set-key (kbd "\C-cl") 'org-store-link)
(global-set-key (kbd "\C-ca") 'org-agenda)
(global-set-key (kbd "\C-cc") 'org-capture)
(global-set-key (kbd "\C-cb") 'org-switchb)

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))

;; Which files open with emacs? Or system default app...
(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.xls\\'" . default))
  (add-to-list 'org-file-apps '("\\.xlsx\\'" . default)))


;; Set to the location of your Org files on your local system
;; use iCloud client on Windows
(if (eq system-type 'windows-nt)
    (setq org-directory (expand-file-name "C:/Users/egronei/iCloudDrive/iCloud~com~appsonthemove~beorg/org/"))
  (setq org-directory (expand-file-name "~/org/")))

(global-visual-line-mode t)

(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-inbox-path (concat org-directory "inbox.org"))
(setq org-gtd-path (concat org-directory "gtd.org"))
(setq org-work-path (concat org-directory "work.org"))
(setq org-tickler-path (concat org-directory "tickler.org"))
(setq org-someday-path (concat org-directory "someday.org"))

;; Org Agenda
(setq org-agenda-files (list org-directory))
(setq org-refile-targets '((org-gtd-path :maxlevel . 1)
                           (org-work-path :maxlevel . 2)
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
                              ("w" "Work Task" entry (file+headline ,(concat org-directory "work.org") "Work")
                               "* TODO %i%?")
                              ("t" "Todo [inbox]" entry (file+headline ,(concat org-directory "inbox.org") ,(format "%s %s" (format-time-string "%Y")(format-time-string "%B")))
                               "* TODO %i%?\n %U")
                              ("m" "Meeting" entry (file+headline ,(concat org-directory "work.org") "Work")
                               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                              ("P" "Phone call" entry (file ,(concat org-directory "work.org") "Work")
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
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((applescript . t)
      (C . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (lua . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (R . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t)))))

;; Path to executables for org-babel
(setq org-ditaa-jar-path "~/repos/notes-hugo/bin/ditaa.jar")
(setq org-plantuml-jar-path "~/repos/notes-hugo/bin/plantuml.jar")

;; Org Roam
(maybe-require-package 'org-roam)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (expand-file-name "~/notes"))
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (setq org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-enable))

;; Set org-roam Keybindings
(global-set-key (kbd "\C-cnl") 'org-roam-buffer-toggle)
(global-set-key (kbd "\C-cnf") 'org-roam-node-find)
(global-set-key (kbd "\C-cni") 'org-roam-node-insert)
(global-set-key (kbd "\C-cnc") 'org-roam-capture)



(unless (version< emacs-version "27.1")
  (require-package 'org-roam-ui))

(setq org-roam-capture-templates '(("d" "default" plain "%?"
                                    :target (file+head "${slug}.org"
                                                       "#+title: ${title}\n")
                                    :unnarrowed t)))
(setq org-roam-dailies-directory "journal/")



;;; Package:
(provide 'init-org)
;;; init-org.el ends here
