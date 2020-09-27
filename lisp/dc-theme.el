(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq custom-safe-themes t)

(use-package solarized-theme
  :ensure
  :config
  (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t)
  (setq solarized-use-less-bold t
	solarized-use-more-italic nil
	solarized-use-variable-pitch t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package circadian
  :ensure t
  :demand
  :config
  (setq circadian-themes '(("8:00" . solarized-light)
                           ("19:30" . solarized-dark)))
  (circadian-setup))

(setq-default line-spacing 5)
(setq-default cursor-type 'bar)
(blink-cursor-mode t)
(setq x-stretch-cursor nil)
(setq x-underline-at-descent-line t)

(use-package minions
  :ensure t
  :demand
  :config
  (minions-mode))

(use-package moody
  :ensure t
  :demand
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq moody-mode-line-height 28))

(defun dc/light-theme-changes ()
  (interactive)
  (dc/theme-changes)
  (set-face-attribute 'default nil
		      :inherit 'fixed-pitch
		      :foreground "black"))
  ;; :foreground "#21505a")

(defun dc/dark-theme-changes ()
  (interactive)
  (dc/theme-changes)
  (set-face-attribute 'default nil
		      :inherit 'fixed-pitch
		      :foreground "white"))
  ;; :foreground "#cccccc")

(defun dc/theme-changes ()
  (interactive)
  (dc/org-theme)
  (dc/jupyter-faces)

  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil
			:height 120)

    (set-face-attribute 'fixed-pitch nil
			:inherit 'default
			:family "Monaco")

    (set-face-attribute 'variable-pitch nil
			:inherit 'default
			:family "Avenir"
			:weight 'regular))

  (when (eq window-system 'x)
    (set-face-attribute 'fixed-pitch nil
			:family "mononoki"
			:height 120)
    (set-face-attribute 'variable-pitch nil
			:family "CMU Sans Serif"
			:weight 'regular
			:height 140))

  ;; (set-face-attribute 'hl-line nil
  ;;  		      :foreground nil
  ;; 		      :background "#eee8d5")

  (set-face-attribute 'header-line nil
		      :background nil
		      :inherit nil)

  (set-face-attribute 'cursor nil
		      :foreground "black"
		      :background nil
		      :inherit nil)

  (set-face-attribute 'font-lock-constant-face nil
		      :bold nil)

  (set-face-attribute 'font-lock-keyword-face nil
		      :bold nil)

  (set-face-attribute 'font-lock-comment-face nil
		      :inherit 'fixed-pitch)

  (set-face-attribute 'font-lock-builtin-face nil
		      :foreground "#b58900"
		      :bold nil)

  (set-face-attribute 'tooltip nil
		      :inherit 'fixed-pitch
		      :foreground "#586e75"
		      :background "#eee8d5"))

(use-package apropospriate-theme
  :disabled)

(provide 'dc-theme)
