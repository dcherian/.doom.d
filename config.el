;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Deepak Cherian"
      user-mail-address "deepak@cherian.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-font (font-spec :family "Fira Code" :size 12))
(setq doom-theme 'doom-wilmersdorf)

(setq-default line-spacing 5)
(setq x-stretch-cursor nil)
(setq x-underline-at-descent-line t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq dash-docs-docsets-path "~/docsets/")

(setq auto-mode-alist (append '(
                              ("\\.f\\'" . fortran-mode)
                              ("\\.f77\\'" . fortran-mode)
                              ("\\.f90\\'" . f90-mode)
                              ("\\.F90\\'" . f90-mode)
                              )
                              auto-mode-alist))

(global-set-key "\C-xw" 'delete-frame)
(global-set-key "\C-c\C-r" 'eval-region)
(global-set-key "\C-c\C-b" 'eval-buffer)
(global-set-key (kbd "C-.") 'just-one-space)
(global-set-key (kbd "M-&") 'replace-string)
(global-set-key (kbd "M-*") 'replace-regexp)
;; below works even when kill-this-buffer doesn't
(global-set-key (kbd "C-x k")
                (lambda () (interactive) (kill-buffer (current-buffer))))

(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)

(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f11>") 'gud-step) ;; equiv matlab step in
(global-set-key (kbd "<f7>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f8>") 'gud-finish) ;; equiv matlab step out
(use-package! wc-mode)

(use-package! transpose-frame
  :defer
  :bind (:map dc/toggle-map
         ("f" . transpose-frame)))

(use-package! magit
  :defer
  :bind (:map dc-bindings-map
         ("C-x g" . magit-status)
         ("C-x l" . magit-log-buffer-file)
         ("C-x d" . magit-log-trace-definition))
  :config
  (setq magit-repository-directories
        '(("~/.doom.d/" . 0)
          ("~/dotfiles/" . 0)
          ("~/python/" . 2)
          ("~/gits/" . 1)))

  (add-to-list 'magit-section-initial-visibility-alist '(untracked . hide))

  (defvar exp-feat/recent-branches (make-hash-table :test 'equal))

  (defcustom exp-feat/recent-branches-limits 5
    "Limits" :type 'integer :risky t)

  (defun exp-feat/magit-insert-recent-branches nil
    "Insert recent branches"
    (let* ((dir (magit-toplevel))
           (curr-branch (magit-get-current-branch))
           (prev-branch (magit-get-previous-branch))
           (rbs (--> (gethash dir exp-feat/recent-branches)
                     (nconc (list prev-branch curr-branch) it)
                     (-distinct it)
                     (-filter (lambda (a) (and a (not (equal a curr-branch)))) it))))
      (when rbs
        (when (> (length rbs) exp-feat/recent-branches-limits)
          (--> (1- exp-feat/recent-branches-limits)
               (nthcdr it rbs)
               (setcdr it nil)))
        (puthash dir rbs exp-feat/recent-branches)
        (magit-insert-section (rb "rb")
          (magit-insert-heading "Recent branches")
          (dolist (it-branch rbs)
            (let ((output (magit-rev-format "%h %s" it-branch)))
              (string-match "^\\([^ ]+\\) \\(.*\\)" output)
              (magit-bind-match-strings (commit summary) output
                (when (and t (equal summary ""))
                  (setq summary "(no commit message)"))
                (magit-insert-section (branch it-branch)
                  (insert (propertize commit
                                      'font-lock-face 'magit-hash) ?\s)
                  (insert (propertize it-branch
                                      'font-lock-face 'magit-branch-local) ?\s)
                  (insert (funcall magit-log-format-message-function
                                   it-branch summary) ?\n)))))))))

  (magit-add-section-hook 'magit-status-sections-hook 'exp-feat/magit-insert-recent-branches 'append t)

  (add-to-list 'magit-buffer-log-args "--follow")
  (magit-auto-revert-mode)
  (setq vc-handled-backends nil))

(use-package! counsel
  :defer
  :bind (:map dc-bindings-map
         ("M-9" . counsel-semantic-or-imenu))
  ;; (unbind-key "C-c C-l" shell-mode-map)
  ;; (bind-key "C-c C-l" #'counsel-shell-history shell-mode-map)
  )

(use-package! crux
  :defer
  :bind (:map dc-bindings-map
         ("C-a" . crux-move-beginning-of-line)
         ("C-c s" . crux-transpose-windows)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c I" . crux-find-user-init-file)
         ("C-S-RET" . crux-smart-open-line-above)
         ("S-RET" . crux-smart-open-line)
         ("C-^" . crux-top-join-lines)
         ("C-x C-i" . crux-ispell-word-then-abbrev))
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package! comment-dwim-2
  :defer
  :bind (:map dc-bindings-map
         ("C-;" . comment-dwim-2)))

;; needs to be matlab not matlab-mode!
;; this is because matlab.el is present, not matlab-mode.el
(use-package! matlab
  :defer
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



;; (autoload 'dired-toggle-read-only "dired" nil t)
;;(define-key dc/toggle-map "w" #'whitespace-mode)
;;
(use-package! tramp
  :defer
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-backup-directory-alist backup-directory-alist)
  ;; (setq tramp-auto-save-directory autosave-dir)

  (add-to-list 'password-word-equivalents "Token_Response")
  (setq tramp-password-prompt-regexp
        (format "^.*\\(%s\\).*:\^@? *"
                (regexp-opt (or (bound-and-true-p password-word-equivalents)
                                '("password" "passphrase"))))))

(use-package! projectile
  :defer
  :bind (:map projectile-mode-map
         ("M-p" . projectile-command-map))
  :config
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))
        projectile-sort-order 'access-time)
  (setq projectile-enable-caching t)
)

;; (use-package! counsel-projectile
;;   :defer
;;   :after counsel
;;   :bind (:map dc-bindings-map
;;          ("C-c C-f" . counsel-projectile-find-file)
;;         ;;            :map projectile-command-map
;;         ;;            ("s" . counsel-projectile-ag)
;;          )
;;   :config
;;   (setq projectile-completion-system 'counsel
;;         projectile-switch-project-action 'counsel-projectile)
;;   (counsel-projectile-mode))

(use-package! poporg
  :defer
  :bind (:map dc-bindings-map
         ("s-l" . 'poporg-dwim)))

(use-package! ivy
  :defer
  :bind (:map dc-bindings-map
         ("C-x b" . 'ivy-switch-buffer)))

(use-package! spell-fu
  :config
  (setq ispell-dictionary "english"
        ispell-personal-dictionary "~/.doom.d/dictionary.pws")
  )

(use-package! goto-last-change
  :defer
    :bind (:map dc-bindings-map
         ("C-x x" . 'goto-last-change)))

(use-package! org
  :defer
  :config
  (setq org-startup-indented t
        org-startup-with-inline-images "inlineimages"
        org-hide-leading-stars t
;;      org-return-follows-link t
;;      org-footnote-define-inline t
;;      org-special-ctrl-a/e t
;;      org-special-ctrl-k t
;;      org-ellipsis "…"
;;      org-log-done t
;;      org-catch-invisible-edits 'smart
;;      org-list-allow-alphabetical t
;;      org-hide-emphasis-markers nil
        org-image-actual-width 680
;;      org-export-in-background nil
;;    org-src-fontify-natively 1
;;      org-src-tab-acts-natively 1
;;      org-src-preserve-indentation t
;;      org-pretty-entities nil
;;      org-pretty-entities-include-sub-superscripts t
        org-export-dispatch-use-expert-ui t
        org-export-time-stamp-file nil
;;      fill-column 90
;;      org-src-window-setup 'current-window
;;      org-export-time-stamp-file nil
        org-imenu-depth 3
        )

  ;; remove comments from org document for use with export hook
  ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun delete-org-comments (backend)
    (cl-loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                                      'comment 'identity))
          do
          (setf (buffer-substring (org-element-property :begin comment)
                                  (org-element-property :end comment))
                "")))
  ;; add to export hook
  (add-hook 'org-export-before-processing-hook 'delete-org-comments)

   )

(use-package! ox-extra
  :after org
  :config
  (ox-extras-activate '(ignore-headlines))
  )

(use-package! org-inlinetask
  :after org
  )


(use-package! ox-latex
  :after org
  :config
  (defun dc/org-latex-word-count ()
    (interactive)
    (org-latex-export-to-latex)
    (shell-command (concat "texcount "
                                        ; "uncomment then options go here "
                           (file-name-sans-extension buffer-file-name)
                           ".tex")))
  (defun dc/org-latex-character-count ()
    (interactive)
    (org-latex-export-to-latex)
    (shell-command (concat "texcount -char "
                                        ; "uncomment then options go here "
                           (file-name-sans-extension buffer-file-name)
                           ".tex")))
  (define-key org-mode-map "\C-cw" 'dc/org-latex-word-count)
  (define-key org-mode-map "\C-cW" 'dc/org-latex-character-count)

  (setq org-latex-hyperref-template nil
        org-latex-listings t
        org-latex-prefer-user-labels t
        org-latex-tables-booktabs t
        org-latex-table-scientific-notation nil
        org-latex-compiler-file-string nil
        org-latex-image-default-width "\\textwidth"
        org-highlight-latex-and-related '(latex script entities)
        org-cite-export-processors nil
        )

  (add-to-list 'org-latex-listings-langs '(ipython "Python"))

  (setq org-latex-pdf-process
        '("source ~/.bashrc; sed '/./,$!d' %f > %f.nolines; mv %f.nolines %f; latexmk %f; exiftool -overwrite_original -Producer=`git rev-parse HEAD` %b.pdf"))
  (load! "lisp/dc-ox-latex-classes"))


(use-package! visual-fill-column
  :defer
  :bind (:map dc/toggle-map
         ("v" . visual-fill-column-mode))
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width 150)
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust))




;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(load! "lisp/dc-website.el")
(load! "lisp/dc-org.el")
(load! "lisp/dc-python.el")

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272C36" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   (quote
    ("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" default)))
 '(fci-rule-color "#5B6268")
 '(global-undo-tree-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-files
   (quote
    ("~/work/pump/paper2/paper2.org" "/home/deepak/org/2fa-backup.org" "/home/deepak/org/Getting Started with Orgzly.org" "/home/deepak/org/bio.org" "/home/deepak/org/boston.org" "/home/deepak/org/bug-report.org" "/home/deepak/org/byomkesh.org" "/home/deepak/org/cli.org" "/home/deepak/org/driving.org" "/home/deepak/org/emacs-learning (Macintosh-193's conflicted copy 2017-01-20).org" "/home/deepak/org/emacs-learning.org" "/home/deepak/org/french-airport.org" "/home/deepak/org/git.org" "/home/deepak/org/hindi-translations.org" "/home/deepak/org/laptops.org" "/home/deepak/org/library-of-babel.org" "/home/deepak/org/linux.org" "/home/deepak/org/loyalty.org" "/home/deepak/org/meeting.org" "/home/deepak/org/nasa-proposal.org" "/home/deepak/org/new-music.org" "/home/deepak/org/notebook.org" "/home/deepak/org/oceanfacts.org" "/home/deepak/org/org-mode-beamer.org" "/home/deepak/org/org.org" "/home/deepak/org/osu-move.org" "/home/deepak/org/papers (Macintosh-193's conflicted copy 2017-01-20).org" "/home/deepak/org/papers.org" "/home/deepak/org/phd.org" "/home/deepak/org/presentations.org" "/home/deepak/org/proposal-notes.org" "/home/deepak/org/python-notes.org" "/home/deepak/org/python.org" "/home/deepak/org/readwatch (Macintosh-193's conflicted copy 2017-01-20).org" "/home/deepak/org/readwatch.org" "/home/deepak/org/recipes.org" "/home/deepak/org/rotary-article.org" "/home/deepak/org/shopping list.org" "/home/deepak/org/sumatra.org" "/home/deepak/org/temp.org" "/home/deepak/org/ting-codes.org" "/home/deepak/org/todo (Macintosh-193's conflicted copy 2017-03-31).org" "/home/deepak/org/todo.org" "/home/deepak/org/todo.sync-conflict-20191104-004549-NWNNVCC.org" "/home/deepak/org/travel.org" "/home/deepak/org/whiskeys.org" "/home/deepak/org/work-folder.org" "/home/deepak/org/writing-advice.org" "/home/deepak/org/xarray.org" "/home/deepak/org/yellowstone.org")))
 '(package-selected-packages
   (quote
    (orgalist poporg counsel-projectile projectile undo-tree counsel transpose-frame)))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values (quote ((org-publish-use-timestamps-flag))))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq auth-sources
    '((:source "~/.authinfo")))

(after! org
  (remove-hook 'after-save-hook #'+literate|recompile-maybe))
