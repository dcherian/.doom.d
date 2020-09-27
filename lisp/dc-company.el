(use-package company
  :diminish company-mode
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  ;; Except when you're in term-mode.
  (setq company-global-modes '(not term-mode))

  (setq company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
	company-dabbrev-code-other-buffers t
        company-dabbrev-downcase nil
	company-dabbrev-ignore-case nil
	company-idle-delay 0.5)

  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))

  (set-face-attribute 'company-tooltip nil
		      :inherit 'tooltip)

  (setq company-backends
	'(company-capf
	  company-files
	  (company-dabbrev-code company-gtags company-etags
				company-keywords)
	  company-dabbrev))

  )

(use-package company-math
  :disabled
  :after (company)
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  (setq company-math-allow-latex-symbols-in-faces t))

;; rank completion candidates by statistics of use
(use-package company-statistics
  :ensure t
  :after (company)
  :config
  (company-statistics-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

(provide 'dc-company)
