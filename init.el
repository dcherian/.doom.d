;;; init.el -*- lexical-binding: t; -*-
;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).
(defvar dc-bindings-map (make-keymap) "A keymap for custom bindings.")

(define-prefix-command 'dc/toggle-map)
(define-key ctl-x-map "t" 'dc/toggle-map)
(define-key dc/toggle-map "c" #'column-number-mode)
(define-key dc/toggle-map "l" #'toggle-truncate-lines)
(define-key dc/toggle-map "r" #'dired-toggle-read-only)
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
        '(("~/.emacs.d/" . 0)
          ("~/dotfiles/" . 0)
          ("~/pods/" . 3)
          ("~/python/" . 2)
          ("~/gits/" . 1)))

  (add-to-list 'magit-section-initial-visibility-alist '(untracked . hide))

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

;; (use-package! undo-tree
;;   :bind (:map dc-bindings-map
;; 	      ("M--" . undo-tree-undo)
;; 	      ("M-=" . undo-tree-redo)
;; 	      ("M-u" . undo-tree-visualize))
;;   :config
;;   (global-undo-tree-mode t))

(use-package! projectile
  :defer
  :bind (:map projectile-mode-map
         ("M-p" . projectile-command-map))
  :config
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))
        projectile-sort-order 'access-time)
  (setq projectile-enable-caching t)
  (projectile-mode))

(use-package! counsel-projectile
  :defer
  :bind (:map dc-bindings-map
         ("C-c C-f" . counsel-projectile-find-file-dwim)
        ;;	      :map projectile-command-map
        ;;	      ("s" . counsel-projectile-ag)
         )
  :config
  (setq projectile-completion-system 'counsel
        projectile-switch-project-action 'counsel-projectile-find-file
        projectile-switch-project-action 'counsel-projectile)
  (counsel-projectile-mode))

(use-package! poporg
  :defer
  :bind (:map dc-bindings-map
         ("s-l" . 'poporg-dwim)))

(use-package! ivy
  :defer
  :bind (:map dc-bindings-map
         ("C-x b" . 'ivy-switch-buffer)))

(use-package! org
  :defer
  :config
  (setq org-startup-indented t
        org-startup-with-inline-images "inlineimages"
        org-hide-leading-stars t
;; 	org-return-follows-link t
;; 	org-footnote-define-inline t
;; 	org-special-ctrl-a/e t
;; 	org-special-ctrl-k t
;; 	org-ellipsis "…"
;; 	org-log-done t
;; 	org-catch-invisible-edits 'smart
;; 	org-list-allow-alphabetical t
;; 	org-hide-emphasis-markers nil
        org-image-actual-width 680
;; 	org-export-in-background nil
;;    org-src-fontify-natively 1
;; 	org-src-tab-acts-natively 1
;; 	org-src-preserve-indentation t
;; 	org-pretty-entities nil
;; 	org-pretty-entities-include-sub-superscripts t
        org-export-dispatch-use-expert-ui t
        org-export-time-stamp-file nil
;; 	fill-column 90
;; 	org-src-window-setup 'current-window
;; 	org-export-time-stamp-file nil
        org-imenu-depth 3
        )

  (require 'ox-extra)
  (require 'org-inlinetask)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
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
        org-highlight-latex-and-related '(latex script entities))

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


(doom! :input
       ;;chinese
       ;;japanese

       :completion
       company           ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;;indent-guides     ; highlighted indent columns
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;pretty-code       ; ligatures or substitute text with pretty symbols
       ;;tabs              ; an tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       ;;(evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp
       ;;macos             ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :lang
       ;;agda              ; types of types of types of types...
       ;;cc                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +jupyter)      ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +conda)  ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))

(after! org-src
  (dolist (lang '(python julia R))
    (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                org-src-lang-modes :key #'car)))

;; do my keybindings
(define-minor-mode dc-bindings-mode
  "A mode that activates dc-bindings."
  :init-value t
  :lighter " dc-keys"
  :keymap dc-bindings-map)

(defun dc-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
   Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'dc-bindings-mode)
    (let ((mykeys (assq 'dc-bindings-mode minor-mode-map-alist)))
      (assq-delete-all 'dc-bindings-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))
(add-hook 'after-load-functions 'dc-keys-have-priority)
