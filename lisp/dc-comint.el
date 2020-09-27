;;; Commentary
;; My comint configuration
;; - Deepak Cherian

;;; Code
;; other comint options
(use-package comint
  :init (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :bind (:map comint-mode-map
	      ("<up>" . comint-previous-matching-input-from-input)
	      ("<down>" . comint-next-matching-input-from-input))
  :config
  (setq comint-scroll-to-bottom-on-input t  ; always insert at the bottom
	comint-scroll-to-bottom-on-output t ; always add output at the bottom
	comint-scroll-show-maximum-output t ; scroll to show max possible output
	comint-input-ignoredups t           ; no duplicates in command history
	comint-completion-autolist t     ; show completion list when ambiguous
	comint-completion-addsuffix t      ; insert space/slash after file completion
	comint-buffer-maximum-size 5000    ; max length of the buffer in lines
	comint-prompt-read-only nil         ; if this is t, it breaks shell-command
	comint-input-ring-size 5000         ; max shell history size
	protect-buffer-bury-p nil)

  ;; interpret and use ansi color codes in shell output windows
  ;; truncate buffers continuously
  (set-face-attribute 'comint-highlight-input nil
		      :inherit 'font-lock-keyword-face :weight 'normal)
  (set-face-attribute 'comint-highlight-prompt nil
		      :inherit 'font-lock-constant-face)

  ;; close comint Completion buffers by default
  ;; from https://snarfed.org/automatically_close_completions_in_emacs_shell_comint_mode
  (defun comint-close-completions ()
    "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
    (if comint-dynamic-list-completions-config
	(progn
	  (set-window-configuration comint-dynamic-list-completions-config)
	  (setq comint-dynamic-list-completions-config nil))))

  (defadvice comint-send-input (after close-completions activate)
    (comint-close-completions))

  (defadvice comint-dynamic-complete-as-filename (after close-completions activate)
    (if ad-return-value (comint-close-completions)))

  (defadvice comint-dynamic-simple-complete (after close-completions activate)
    (if (member ad-return-value '('sole 'shortest 'partial))
	(comint-close-completions)))

  (defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (if (not unread-command-events)
	;; comint's "Type space to flush" swallows space. put it back in.
	(setq unread-command-events (listify-key-sequence " "))))

  ;; ;; make shell output read only - Messes with matlab-shell :()
  ;; (defvar my-shells
  ;;     '("*shell0*" "*shell1*" "*MATLAB*"))
  ;; (defun make-my-shell-output-read-only (text)
  ;;   "Add to comint-output-filter-functions to make stdout read only in my shells."
  ;;   (if (member (buffer-name) my-shells)
  ;;       (let ((inhibit-read-only t)
  ;;             (output-end (process-mark (get-buffer-process (current-buffer)))))
  ;;         (put-text-property comint-last-output-start output-end 'read-only t))))
  ;; (add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)
  )

(provide 'dc-comint)
