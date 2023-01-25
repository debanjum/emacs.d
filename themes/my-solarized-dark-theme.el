(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(setq my-solarized-dark-color-palette-alist
      (append
       '((dark-grey . "#96A7A9")
         (grey . "#405A61"))
       solarized-dark-color-palette-alist))

(deftheme my-solarized-dark "My solarized dark colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'dark 'my-solarized-dark
  my-solarized-dark-color-palette-alist
  '((custom-theme-set-faces
     theme-name
     ;; Base styling
     `(link ((,class (:foreground ,blue :underline t :weight normal))))
     `(cursor ((,class (:foreground ,base03 :background ,blue :inverse-video t))))

     ;; Org Styling
     `(org-headline-done ((,class (:foreground ,grey))))
     `(org-done ((,class (:foreground ,grey :strike-through nil :weight bold))))
     `(org-drawer ((,class (:foreground ,dark-grey))))
     `(org-date ((,class (:foreground ,blue :underline nil))))
     `(org-tag ((,class (:weight normal :foreground ,yellow))))
     `(org-habit-overdue-face ((,class (:background ,orange :foreground ,orange-d))))
     `(org-habit-overdue-future-face ((,class (:background ,orange-l))))
     `(org-scheduled-previously ((,class (:foreground ,orange))))
     `(org-warning ((,class (:foreground ,orange-d))))
     )))

(provide-theme 'my-solarized-dark)
(provide 'my-solarized-dark-theme)
