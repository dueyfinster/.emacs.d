;;; init-java.el --- Java support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'eglot-java)
(add-hook 'java-mode-hook 'eglot-java-mode)
(require-package 'java-snippets)
(require-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;;; Package:
(provide 'init-java)
;;; init-java.el ends here
