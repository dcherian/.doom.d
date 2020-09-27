(use-package ibuffer
  :bind (:map dc-bindings-map
	      ("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("rama" (filename . "rama"))
		 ("pump" (filename . "pump"))
		 ("ebob" (filename . "ebob"))
		 ("magit" (filename . "magit"))
		 ("eq-waves" (filename . "eq_waves"))
		 ("eddyshelf" (filename . "eddyshelf"))
		 ("dired" (mode . dired-mode))
		 ("org" (name . "^.*org$"))
		 ("helm" (name . "helm"))
		 ("programming" (or
				 (mode . python-mode)
				 (mode . matlab-mode)
				 (mode . jupyter-repl-mode)))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))
		 ))))

  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "default")))

  (add-to-list 'ibuffer-fontification-alist '(5 buffer-file-name 'font-lock-keyword-face))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))

  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil))

(provide 'dc-ibuffer)
