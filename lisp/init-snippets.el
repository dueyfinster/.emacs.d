;;; init-projectile.el --- Use Snippets to aid in autocompletion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode)
  (defun gen-cpp-header-tag()
    (let* ((root (projectile-project-root))
           (path (string-trim-left
                  (file-name-sans-extension (buffer-file-name))
                  root))
           (name (replace-regexp-in-string "[./-]" "_" path)))
      (concat (upcase name) "_H_"))))


;;; Package:
(provide 'init-snippets)
;;; init-snippets.el ends here
