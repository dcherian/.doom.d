;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

;; needs to be matlab not matlab-mode!
;; this is because matlab.el is present, not matlab-mode.el
(use-package matlab
  :init (defvar default-fill-column 200)
  :commands dc/matlab-shell-other-window
  :hook (matlab-shell . comint-read-input-ring)
  :bind (:map matlab-mode-map
	      ("C-c C-m" . matlab-shell)
	      ("C-c C-a" . matlab-shell-run-cell)
	      ("C-c C-c" . matlab-shell-run-region-or-line)
	      ("C-c C-o" . dc/matlab-shell-other-window))
  :config
  (defun dc/matlab-shell-other-window ()
    (interactive)
    (other-window 1)
    (matlab-shell))

  (setq matlab-shell-command "matlab"
	matlab-indent-function-body t
	matlab-functions-have-end t
	matlab-verify-on-save-flag nil
	matlab-shell-command-switches '("-nodesktop -nosplash")
	matlab-mode-verify-fix-functions nil
	matlab-shell-history-file "~/.matlab/R2018a/history.m"))

(provide 'dc-matlab)
