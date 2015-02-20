;; -*- mode: emacs-lisp -*-
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

; -----------------------------------------------------------------------------
;; auto customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#272935" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#f4f1ed"] t)
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" default)))
 '(global-whitespace-mode t)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(whitespace-display-mappings (quote ((space-mark 32 [46]) (tab-mark 9 [124 45])))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-other-screen-face ((t (:background "gray15" :foreground "white" :underline t))))
 '(hl-line ((t (:background "color-235")))))
;; -----------------------------------------------------------------------------

(defun require-package (package)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'ample-theme)
(require-package 'cider)
(require-package 'clojure-mode)
(require-package 'company)
(require-package 'company-ghc)
(require-package 'company-tern)
(require-package 'elm-mode)
(require-package 'emmet-mode)
(require-package 'evil)
(require-package 'evil-easymotion)
(require-package 'evil-jumper)
(require-package 'evil-leader)
(require-package 'evil-nerd-commenter)
(require-package 'evil-surround)
(require-package 'evil-tabs)
(require-package 'exec-path-from-shell)
(require-package 'fill-column-indicator)
(require-package 'flycheck)
(require-package 'flycheck-haskell)
(require-package 'flycheck-rust)
(require-package 'fsharp-mode)
(require-package 'git-gutter+)
(require-package 'google-this)
(require-package 'haskell-mode)
(require-package 'helm)
(require-package 'helm-dash)
(require-package 'helm-hayoo)
(require-package 'helm-hoogle)
(require-package 'helm-itunes)
(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'jsx-mode)
(require-package 'json-mode)
(require-package 'nlinum)
(require-package 'magit)
(require-package 'projectile)
(require-package 'rainbow-delimiters)
(require-package 'rust-mode)
(require-package 'scss-mode)
(require-package 'skewer-mode)
(require-package 'shm)
(require-package 'slime)
(require-package 'smart-mode-line)
(require-package 'tern)
(require-package 'yasnippet)

; Tidal
;(setq load-path (cons "~/program/github.com/yaxu/Tidal/" load-path))
;(require 'tidal)
;(setq tidal-interpreter "/usr/bin/ghci")

;; Basic Settings
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(tooltip-mode -1)
(setq redisplay-dont-pause t)
(setq debug-on-error nil)

(require 'exec-path-from-shell) ; load "$PATH" from zsh
(add-hook 'after-init-hook 'exec-path-from-shell-initialize)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode) ; auto-completion
(setq company-idle-delay 0)
; undo layout changes
(define-globalized-minor-mode global-winner-mode winner-mode
  (lambda () (winner-mode 1)))
(add-hook 'after-init-hook 'global-winner-mode) ; layout changes undo/redo
; copy/paste/cut commands with reasonable bindings
(add-hook 'after-init-hook
          (lambda () cua-mode 1))
(show-paren-mode 1) ; highlight matching parens on hover
(electric-pair-mode 1) ; auto-insert matching parentheses/brackets/etc.
(setq-default indent-tabs-mode nil) ; use spaces, not tabs
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(ido-mode 1) ; more interactivity
(evil-leader/set-key "j" 'helm-imenu)

; Expand region
(require-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-;") #'er/expand-region)

; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Line numbers
(require 'nlinum)
(add-hook 'prog-mode-hook (lambda () (nlinum-mode 1)))
(setq nlinum-format "%d ")

; Fix crashing on PDF viewing
(add-hook 'doc-view-mode-hook (lambda () (nlinum-mode -1)))
(defvar is-gui (display-graphic-p) "Whether we're running graphical Emacs")
; Better buffer navigation
(global-set-key (kbd "C-x C-h") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)

;; Magit
(add-hook 'magit-commit-mode-hook 'flyspell-mode)

;; Evil mode
(require 'evil)
(require 'evil-easymotion)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'evil-surround)
(evil-mode 1)
(global-evil-leader-mode 1)
(global-evil-surround-mode)
(set-variable 'evil-esc-delay 0) ; Fix escape delays

(evil-leader/set-leader ",")
(evil-leader/set-key "c" 'evilnc-comment-operator)
(evilem-default-keybindings "SPC")
(evil-leader/set-key "e" 'flycheck-list-errors)

; Switch H with ^ and L with $
(define-key evil-motion-state-map "H" 'evil-first-non-blank)
(define-key evil-motion-state-map "L" 'end-of-line)
(evil-leader/set-key "H" 'evil-window-top)
(evil-leader/set-key "L" 'evil-window-bottom)

; Easy tabs switching
(require 'elscreen)
(require 'evil-tabs)
(global-evil-tabs-mode 1)
(evil-leader/set-key "tt" 'elscreen-create)
(evil-leader/set-key "tn" 'elscreen-next)
(evil-leader/set-key "tc" 'elscreen-kill)
(evil-leader/set-key "tp" 'elscreen-previous)
(setq elscreen-prefix-key "")
(setq elscreen-tab-display-control nil)

(evil-leader/set-key "[n" 'nlinum-mode)
(evil-leader/set-key "[g" 'git-gutter+-mode)
(evil-leader/set-key "[s" 'flyspell-mode)
(evil-leader/set-key "gs" 'magit-status)

;; Make the interface as bare as possible
(when is-gui (scroll-bar-mode 0))
(menu-bar-mode -1)
(tool-bar-mode -1)

; Google things
(evil-leader/set-key "ig" 'google-this-noconfirm)

;; Looks
(load-theme 'ample)
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :background "color-235")
(set-terminal-coding-system 'utf-8-auto-unix)

; Highlight trailing whitespace
(require 'whitespace)
(setq whitespace-display-mappings
    ; 32 SPACE, 183 MIDDLE DOT  · , 46 FULL STOP  .
  '((space-mark 32 [183] [46])
    ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE
    (tab-mark 9 [187 9] [9655 9] [92 9])))
(setq whitespace-style '(face tabs trailing tab-mark))
(set-face-attribute 'whitespace-tab nil
                    ;:background "black"
                    :foreground "#00a8a8"
                    :weight 'bold)
(set-face-attribute 'whitespace-trailing nil
                    :background "#e4eeff"
                    :foreground "#183bc8"
                    :weight 'normal)
(global-whitespace-mode 1)

; Set-up the browser as T
(setq browse-url-browser-function 'browse-url-default-browser)
(defun w3m-browse-url-other-window (url &optional new-window)
  "Opens an URL in W3M in a different window"
  (let ((w3m-pop-up-windows t)
        (target-window (if (one-window-p)
                           (split-window)
                         (get-lru-window))))
    (with-selected-window target-window
      (w3m-browse-url url new-window))))

(defun switch-default-browser ()
  "Switches the browse-url function between internal and external browsers"
  (interactive)
  (let ((use-w3m (equal browse-url-browser-function
                        'browse-url-default-browser)))
    (if use-w3m
        (setq browse-url-browser-function 'w3m-browse-url-other-window)
      (setq browse-url-browser-function 'browse-url-default-browser))
    (message "Browser set to: %s" (if use-w3m "W3M" "External Browser"))))
(evil-leader/set-key "bs" 'switch-default-browser)

; Helm Dash
(require 'helm-dash)
(evil-leader/set-key "mf" 'helm-dash)

(setq helm-dash-docsets-path "~/.docsets/")
(defvar helm-dash-docsets)
(defun helm-setup-docsets (hook docsets)
  (add-hook hook `(lambda () (setq-local helm-dash-common-docsets ',docsets))))

(helm-setup-docsets 'haskell-mode-hook '("Haskell"))
(helm-setup-docsets 'emacs-lisp-mode-hook '("Emacs Lisp"))
(helm-setup-docsets 'js2-mode-hook
                    '("Javascript" "Lo-Dash" "EmberJS" "NodeJS" "MarionetteJS"
                      "BackboneJS" "Chai"))
(helm-setup-docsets 'coffee-mode-hook
                    '("jQuery" "jQuery UI" "BackboneJS" "NodeJS" "EmberJS"
                      "MarionetteJS" "CoffeeScript" "Chai" "MomentJS"))
(helm-setup-docsets 'html-mode-hook
                    '("Sass" "CSS" "HTML" "Compass"))
(helm-setup-docsets 'css-mode-hook
                    '("Sass" "CSS" "HTML" "Compass"))
(helm-setup-docsets 'scss-mode-hook
                    '("Sass" "CSS" "HTML" "Compass"))

; Highlight 79th column
(require 'fill-column-indicator)
(setq-default fill-column 79)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

; A better mode-line
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)

;; Persistent undo
(setq undo-tree-history-directory-alist
  `(("." . ,(expand-file-name "~/.emacs.d/undo/"))))
(setq undo-tree-auto-save-history 1)

;; Helm mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Projectile mode
(require 'projectile)
(projectile-global-mode 1)
(require 'helm-projectile)
(helm-projectile-on)
(evil-leader/set-key "ag" 'helm-projectile-ag)

;; Haskell mode
(require 'projectile)
(require 'haskell-mode)
(require 'haskell-indentation)
(add-hook 'haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-indentation-ifte-offset 4)
(setq haskell-indentation-layout-offset 4)
(setq haskell-indentation-left-offset 4)
(setq haskell-process-type (quote cabal-repl))
(evil-leader/set-key-for-mode 'haskell-mode "mt" 'haskell-process-do-type)
(evil-leader/set-key-for-mode 'haskell-mode "mi" 'haskell-process-do-info)
(evil-leader/set-key-for-mode 'haskell-mode "mk" 'haskell-process-cabal)

;; FSharp mode
(require 'fsharp-mode)
(evil-leader/set-key-for-mode 'fsharp-mode "mk" 'fsharp-eval-region)

;; Markup and CSS modes
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; JavaScript mode
(require 'js2-mode)
(require 'company)
(add-hook 'js-mode-hook 'js2-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook '(lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'js2-mode-hook 'skewer-mode)
(setq js2-basic-offset 2)
(setq js2-highlight-level 3)

(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-to-list 'company-backends 'company-tern)

;; JSX mode
(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(setq jsx-indent-level 2)

;; Flycheck mode
(require 'flycheck)
(global-flycheck-mode 1)
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
(setq flycheck-display-errors-delay 0.3)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-indication-mode 'left-fringe)

(setq-default flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;; save backups to the temporary directory
(setq backup-directory-alist
  `((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/auto-save" t)))

(put 'upcase-region 'disabled nil)

(provide '.emacs)
;;; emacs ends here
