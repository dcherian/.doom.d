;;; Commentary:
;; This contains my auctex/latex configuration
;; - Deepak Cherian

;;; Code:

;; make \left \right etc less visible.
(defface endless/unimportant-latex-face
  '((t :height 0.7
       :inherit font-lock-comment-face))
  "Face used on less relevant math commands.")

(font-lock-add-keywords
 'LaTeX-mode
 `((,(rx (or (and "\\" (or (any ",.!;$")
			   (and (or "left" "right"
				    "big" "Big"
				    "text" "limits")
				symbol-end)))
	     (any "_^")))
    0 'endless/unimportant-latex-face prepend))
 'end)

(defun dc/latex-word-count ()
  (interactive)
  (shell-command (concat "texcount "
					; "uncomment then options go here "
			 (file-name-sans-extension buffer-file-name)
			 ".tex")))
;; (define-key latex-mode-map "\C-cw" 'dc/latex-word-count)

(setq-default TeX-PDF-mode nil
	      TeX-master nil)

(setq TeX-auto-save nil
      TeX-parse-self t
      TeX-source-correlate-method 'synctex)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\(\\\\block\\)\\s-*{" 1 font-lock-warning-face t)))))

(provide 'dc-latex)
;;; dc-latex.el ends here
