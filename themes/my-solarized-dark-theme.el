(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme my-solarized-dark "My solarized dark colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'dark 'my-solarized-dark
  solarized-dark-color-palette-alist
  '((custom-theme-set-faces
     theme-name
     ;; Base styling
     `(link ((,class (:foreground ,blue :underline t :weight normal))))
     `(cursor ((,class (:foreground ,base03 :background ,blue :inverse-video t))))

     ;; Org Styling
     `(org-date ((,class (:foreground ,blue :underline nil))))
     `(org-tag ((,class (:weight normal :foreground ,yellow))))
     `(org-habit-overdue-face ((,class (:background ,orange :foreground ,orange-d))))
     `(org-habit-overdue-future-face ((,class (:background ,orange-l))))
     `(org-scheduled-previously ((,class (:foreground ,orange))))
     `(org-warning ((,class (:foreground ,orange-d))))
     )))

(provide-theme 'my-solarized-dark)
(provide 'my-solarized-dark-theme)
