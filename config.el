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
(setq doom-theme 'doom-nord)

(setq-default line-spacing 5)
(setq x-stretch-cursor nil)
(setq x-underline-at-descent-line t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
