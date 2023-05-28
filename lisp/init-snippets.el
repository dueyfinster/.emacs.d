;;; init-projectile.el --- Use Snippets to aid in autocompletion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config (setq yas-snippet-dirs
                '("~/.emacs.d/snippets" ;; local snippets
                  ))
  (yas-global-mode 1))


;;; Package:
(provide 'init-snippets)
;;; init-snippets.el ends here
