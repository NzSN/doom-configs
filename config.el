;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "NzSN"
      user-mail-address "nzsn0101@gmail.com")

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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq org-roam-directory "~/Notes/Concepts")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Encryption Settings
(require 'epa-file)
(epa-file-enable)
;; Encrypting Specific Entries in an org File with org-crypt.
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
;; Org-roam v2 configures
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))



;; Magit ediff
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; banner
(setq fancy-splash-image (expand-file-name "~/.config/doom/misc/splash/cute-demon.png"))

;; VUE
;; (add-hook 'vue-mode-hook #'lsp!)
;; (use-package! lsp-volar)

(setq +format-with-lsp nil)

;; org-mode latex chinese support
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-compiler "xelatex"))

(setq lsp-idle-delay 0.500)


;; cc-mode style
(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 2)))
(defconst my-c-style
  '("c-mode"
    (c-offsets-alist . ((innamespace . [0])))

    (c-basic-offset . 2)))
(c-add-style "my-cc-style" my-cc-style)
(c-add-style "my-c-style" my-c-style)

(defun my-cc-indent-setup ()
  (c-set-style "my-cc-style"))
(add-hook 'c++-mode-hook 'my-cc-indent-setup)
(defun my-c-indent-setup ()
  (c-set-style "my-c-style"))
(add-hook 'c-mode-hook 'my-c-indent-setup)


;; keybindings
(map! :leader 
      :desc "Code Tree"
      "c u" #'lsp-ui-imenu)
(load-library "find-lisp")
(use-package! org
  :defer t
  :config
  ;; Setup attach for Notes
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-files
        (append (find-lisp-find-files "~/Notes" "\.org$")
                (find-lisp-find-files "~/Notes" "\.org\.gpg$")))
  (setq org-agenda-text-search-extra-files
        '(agenda-archives
          "~/Notes/Schedules/Schedule.org.gpg"))
  (setq org-attach-directory "~/Notes/.attach")
  (setq org-attach-id-dir    "~/Notes/.attach")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s!)" "WAIT(w!)" "HOLD(h!)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D!)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))))

;; Read env from .zshrc into emacs
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; lsp mode
(setq lsp-clients-clangd-args '("-j=12"
				"--background-index"
				"--clang-tidy"
                                "--all-scopes-completion"
				"--completion-style=detailed"
				"--header-insertion=iwyu"
				"--pch-storage=disk"))
;; Use ccls instead. Uncomment this if want ti use clangd.
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; (map! "C-c C" #'centaur-tabs--kill-this-buffer-dont-ask)

(load! "lisp/kaolin-themes/kaolin-themes-lib")
(load! "lisp/kaolin-themes/kaolin-themes")
(load! "lisp/kaolin-themes/kaolin-themes-treemacs")
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))

(load! "lisp/tla-tools/tla-pcal-mode")
(load! "lisp/tla-tools/tla-tools")
(require 'tla-pcal-mode)
(require 'tla-tools)
(use-package tla-pcal-mode :mode "\.tla$")

(load! "lisp/org-fragtog/org-fragtog")
(require 'org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(load! "lisp/bazel")
(require 'bazel)
(use-package bazel-mode :mode "\.bzl$")
(use-package bazel-mode :mode "\.bazel$")

;; You can use this hydra menu that have all the commands
(map! :n "C-SPC" 'harpoon-quick-menu-hydra)
(map! :n "C-s" 'harpoon-add-file)

(use-package! highlight-indent-guides
     :defer t
     :config
     (setq highlight-indent-guides-method 'column))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(setq org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\12")
      :unnarrowed t)))

;; Setup background for inline images in org-mode.
(defun org--create-inline-image-advice (img)
  (nconc img (list :background "#eeeee4")))
(advice-add 'org--create-inline-image
            :filter-return #'org--create-inline-image-advice)

;; Select theme based on time
;; (defun synchronize-theme ()
;;   (let* ((light-theme 'doom-solarized-light)
;;          (dark-theme  'doom-solarized-dark)
;;          (start-time-light-theme 6)
;;          (end-time-light-theme 18)
;;          (hour (string-to-number (substring (current-time-string) 11 13)))
;;          (next-theme (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
;;                          light-theme dark-theme)))
;;     (when (not (equal doom-theme next-theme))
;;       (setq doom-theme next-theme)
;;       (load-theme next-theme t)
;;       (custom-theme-save))))

;; (run-with-timer 0 900 'synchronize-theme)

;; super-save
(use-package! super-save
  :after org
  :ensure t
  :config
  (super-save-mode +1)
  (setq auto-save-default t)
  (setq auto-save-default nil)
  (setq super-save-silent t)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  )

;; protobuf-mode
(load! "lisp/protobuf-mode")
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(use-package protobuf-mode
  :mode ".proto$"
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))
