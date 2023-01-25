(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(setq my-solarized-light-color-palette-alist
      (append
       '((dark-grey . "#96A7A9")
         (grey . "#D6D6D6"))
       solarized-light-color-palette-alist))

(deftheme my-solarized-light "My solarized light colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'light 'my-solarized-light
  my-solarized-light-color-palette-alist
  '((custom-theme-set-faces
     theme-name
     ;; Base styling
     `(link ((,class (:foreground ,blue :underline t :weight normal))))
     `(cursor ((,class (:foreground ,base03 :background ,blue :inverse-video t))))

     ;; Org Styling
     `(org-headline-done ((,class (:foreground ,dark-grey))))
     `(org-done ((,class (:foreground ,grey :strike-through nil :weight bold))))
     `(org-drawer ((,class (:foreground ,dark-grey))))
     `(org-date ((,class (:foreground ,blue :underline nil))))
     `(org-tag ((,class (:weight normal :foreground ,yellow))))
     `(org-habit-overdue-face ((,class (:background ,orange :foreground ,orange-d))))
     `(org-habit-overdue-future-face ((,class (:background ,orange-l))))
     `(org-scheduled-previously ((,class (:foreground ,orange))))
     `(org-warning ((,class (:foreground ,orange-d))))
     )))

(provide-theme 'my-solarized-light)
(provide 'my-solarized-light-theme)
