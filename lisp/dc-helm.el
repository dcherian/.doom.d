(use-package helm-flx
  :ensure t
  :init
  (helm-flx-mode 1)
  ;; garbage collection
  (defun eos/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun eos/minibuffer-exit-hook ()
    ;; 20mb
    (setq gc-cons-threshold (* 20 1024 1024)))

  (add-hook 'minibuffer-setup-hook #'eos/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'eos/minibuffer-exit-hook))

(use-package helm-config
  :ensure helm
  :demand t
  :init
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :bind (("M-x" . helm-M-x)
	 ("C-q" . helm-M-x)
	 ;; This needs to be here because undo-tree-mode
	 ;; auto-activates only if this is over-ridden in global-map
	 ("C-/" . helm-mini)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
	 ("C-z" .  helm-select-action) ; list actions using C-z
	 :map shell-mode-map
	 ("C-<up>" . helm-comint-input-ring)
	 :map comint-mode-map
	 ("C-<up>" . helm-comint-input-ring)
	 :map dc-bindings-map
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-mini)
	 ("C-`" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("C-x m" . helm-man-woman)
	 ("C-h o" . helm-occur)
	 ("C-h a" . helm-apropos)
	 ("C-h t" . helm-world-time)
	 ("C-M-s" . helm-imenu-in-all-buffers))
  :config
  (helm-mode 1)
  (helm-autoresize-mode nil)
  ;;  (helm-linum-relative-mode 1)

  ;; Fuzzy matching for everything
  (setq helm-M-x-fuzzy-match t
	helm-recentf-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-locate-fuzzy-match nil
	helm-mode-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t)

  ;; Work with Spotlight on OS X instead of the regular locate
  (if (eq window-system 'darwin)
      (setq helm-locate-command "mdfind -name -onlyin ~ %s %s"))

  (setq helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source t ; move to end/beginning of source when reaching top/bottom of source.
	helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-display-header-line nil ; remove header lines if only a single source
	helm-echo-input-in-header-line t ; provide input in the header line and hide the mode lines above
	helm-ff-file-name-history-use-recentf t
	helm-ff-skip-boring-files t
	helm-echo-input-in-header-line t
	helm-google-suggest-use-curl-p t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 50))

(use-package helm-swoop
  :ensure t
  :after (helm)
  :bind (:map dc-bindings-map
	      ("C-s" . helm-swoop)
	      ("M-I" . helm-swoop-back-to-last-point)
	      ("C-c M-i" . helm-multi-swoop)
	      :map helm-swoop-map
	      ("C-r" . helm-previous-line)
	      ("C-s" . helm-next-line)
	      :map helm-multi-swoop-map
	      ("M-i" . helm-multi-swoop-all-from-helm-swoop)
	      ("C-r" . helm-previous-line)
	      ("C-s" . helm-next-line)
	      :map isearch-mode-map
	      ;; When doing isearch, hand the word over to helm-swoop
	      ("M-i" . helm-swoop-from-isearch))
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
	helm-swoop-use-fuzzy-match t
	;; If this value is t, split window inside the current window
	helm-swoop-split-with-multiple-windows t
	;; Split direcion. 'split-window-vertically or 'split-window-horizontally
	helm-swoop-split-direction 'split-window-vertically
	;; If nil, you can slightly boost invoke speed in exchange for text color
	helm-swoop-speed-or-color t))

(use-package kaomoji
  :ensure t
  :bind (:map dc-bindings-map
	      ("C-c h k" . kaomoji)))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode 1))

(use-package helm-ag
  :ensure t
  :bind (:map dc-bindings-map
	      ("M-s" . helm-ag))
  :config
  (setq helm-ag-command
	"rg --smart-case -M 60 --no-heading --line-number %s %s %s"))

(use-package helm-unicode
  :ensure t
  :after helm
  :bind (:map dc-bindings-map
	      ("C-h u" . helm-unicode)))

(use-package helm-describe-modes
  :ensure t
  :after helm
  :config
  (global-set-key [remap describe-mode] #'helm-describe-modes))

(use-package helm-fuzzier
  :ensure t
  :after helm
  :init
  (helm-fuzzier-mode))

(use-package helm-ext
  :after helm
  :ensure t
  :config
  (helm-ext-ff-enable-skipping-dots t))

(use-package helm-dash
  :ensure t
  :after (:all helm python)
  :hook ((org-mode . org-doc)
	 ((python-mode python-shell-first-prompt jupyter-repl-mode-hook) . python-doc))
  :bind (:map dc-bindings-map
	      ("C-," . helm-dash-at-point))
  :config
  (setq helm-dash-docsets-path "/home/deepak/docs/docsets/")
  (defun python-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Python_3"
				    "NumPy" "SciPy" "Matplotlib"
				    "Pandas" "xarray" "Seaborn"
				    "pytest")))

  (defun org-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Org_Mode" "Python_3" "HTML"
				    "NumPy" "SciPy" "Matplotlib"
				    "CSS" "LaTeX" "Pandas" "xarray"
				    "Seaborn"))))

(provide 'dc-helm)
