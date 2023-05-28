;;; init-fonts.el --- Fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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


(provide 'init-fonts)
;;; init-fonts.el ends here
