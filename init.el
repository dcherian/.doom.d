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
       zen               ; distraction-free coding or writing

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
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :os
       macos             ; MacOS-specific commands
       tty               ; terminal support

       :tools
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup +docsets)   ; navigate your code and its documentation
       (lsp +eglot)
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
        ;;pdf               ; pdf enhancements
        ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
        ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :lang
       emacs-lisp        ; drown in parentheses
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org +jupyter +pretty)      ; Organize your plain life in plain text
       (python +lsp +conda)  ; beautiful is better than ugly
       rst               ; ReST in peace
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       yaml              ; JSON, but readable

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
