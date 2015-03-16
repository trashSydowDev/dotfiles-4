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
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" default)))
 '(global-whitespace-mode t)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/notes-git/toggl.org" "~/notes-git/main.org" "~/notes-git/ecs-deployment.org" "~/notes-git/ember-js.org" "~/notes-git/nix.org")))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-process-type . cabal-repl))))
 '(whitespace-display-mappings (quote ((space-mark 32 [46]) (tab-mark 9 [124 45])))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(col-highlight ((t (:background "color-19"))))
 '(elscreen-tab-other-screen-face ((t (:background "gray15" :foreground "white" :underline t))))
 '(hl-line ((t (:background "color-19"))))
 '(shm-current-face ((t (:background "black"))))
 '(shm-quarantine-face ((t (:background "color-235")))))
;; -----------------------------------------------------------------------------

(defun require-package (package)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'ample-theme)
(require-package 'cider)
(require-package 'clojure-mode)
(require-package 'col-highlight)
(require-package 'company)
(require-package 'company-ghc)
(require-package 'company-tern)
(require-package 'edit-server)
(require-package 'edit-server-htmlize)
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
(require-package 'git-messenger)
(require-package 'google-this)
(require-package 'haskell-mode)
(require-package 'helm)
(require-package 'helm-dash)
(require-package 'helm-hayoo)
(require-package 'helm-hoogle)
(require-package 'helm-itunes)
(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'json-mode)
(require-package 'jsx-mode)
(require-package 'magit)
(require-package 'nlinum)
(require-package 'projectile)
(require-package 'rainbow-delimiters)
(require-package 'rust-mode)
(require-package 'scss-mode)
(require-package 'shm)
(require-package 'skewer-mode)
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
(evil-leader/set-key "ds" 'delete-trailing-whitespace)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(defun history-switch-to-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun history-switch-to-next-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) -1)))

(global-set-key (kbd "C-x C-p") 'history-switch-to-prev-buffer)
(global-set-key (kbd "C-x C-n") 'history-switch-to-next-buffer)

(require 'dired)
(require-package 'dired-subtree)
(require 'dired-subtree)
(define-key dired-mode-map "i" 'dired-subtree-insert)
(define-key dired-mode-map "c" 'dired-subtree-remove)

(require 'exec-path-from-shell) ; load "$PATH" from zsh
(add-hook 'after-init-hook 'exec-path-from-shell-initialize)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode) ; auto-completion
(setq company-idle-delay 0)
(evil-leader/set-key "ya" 'helm-yas-complete)

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
(evil-leader/set-key "," 'helm-imenu) ; Redundancy for SHM mode

(evil-leader/set-key "zt" 'yafolding-toggle-element)
(evil-leader/set-key "za" 'yafolding-toggle-all)

; Expand region
(require-package 'expand-region)
(require 'expand-region)
(evil-leader/set-key ";" #'er/expand-region)

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
(evil-leader/set-key "[t" 'toggle-truncate-lines)
(evil-leader/set-key "[s" 'flyspell-mode)
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "ghh" 'github-browse-file)
(evil-leader/set-key "ghb" 'github-browse-file-blame)
(evil-leader/set-key "gm" 'git-messenger:popup-message)

;; Make the interface as bare as possible
(when is-gui (scroll-bar-mode 0))
(menu-bar-mode -1)
(tool-bar-mode -1)

; Google things
(evil-leader/set-key "ig" 'google-this-noconfirm)

;; Looks
(load-theme 'ample)
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :background "gray-19")
(set-terminal-coding-system 'utf-8-auto-unix)
(require 'col-highlight)
(evil-leader/set-key "[h" 'toggle-highlight-column-when-idle)
(col-highlight-set-interval 0)

; Highlight trailing whitespace
(require 'whitespace)
(setq whitespace-display-mappings
    ; 32 SPACE, 183 MIDDLE DOT  � , 46 FULL STOP  .
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
(defun helm-setup-docsets (hook docsets)
  (add-hook hook `(lambda ()
                    (setq-local helm-dash-common-docsets ',docsets)
                    (setq helm-current-buffer (current-buffer)))))

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

; Function for updating cabal docsets with `dash-haskell`
(defun helm-dash-cabal-update-docsets ()
  (interactive)
  (let* ((cabal-file (haskell-cabal-find-file))
         (command (format "dash-haskell -c %s -o ~/.docsets" cabal-file)))
    (shell-command command)))


; Highlight 79th column
(require 'fill-column-indicator)
(setq-default fill-column 79)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(evil-leader/set-key "[f" 'fci-mode)

; A better mode-line
(require 'server)
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)
; Display the server name in the modeline
(add-hook 'after-init-hooko
          (lambda () (setq mode-line-front-space (concat server-name))))

;; Persistent undo
(setq undo-tree-history-directory-alist
  `(("." . ,(expand-file-name "~/.emacs.d/undo/"))))
(setq undo-tree-auto-save-history 1)

;; Helm mode
(global-set-key (kbd "C-x M-x") 'execute-extended-command)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'remember)
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
(require 'shm)
(require 'shm-case-split)
(define-key shm-map (kbd ")") nil)
(define-key shm-map (kbd "]") nil)
(define-key shm-map (kbd "}") nil)
(add-hook 'haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
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
(evil-leader/set-key-for-mode 'haskell-mode "mk" 'ha/kell-process-cabal)
(evil-leader/set-key-for-mode 'haskell-mode "h" 'shm/backward-node)
(evil-leader/set-key-for-mode 'haskell-mode "l" 'shm/forward-node)
(evil-leader/set-key-for-mode 'haskell-mode "p" 'shm/goto-parent)
(evil-leader/set-key-for-mode 'haskell-mode "n" 'shm/goto-parent-end)
(evil-leader/set-key-for-mode 'haskell-mode "r" 'shm/raise)
(evil-leader/set-key-for-mode 'haskell-mode "s" 'shm/delete-indentation)
(evil-leader/set-key-for-mode 'haskell-mode "y" 'shm/yank)
(evil-leader/set-key-for-mode 'haskell-mode "d" 'shm/kill-node)
(evil-leader/set-key-for-mode 'haskell-mode "D" 'shm/kill-line)
(evil-leader/set-key-for-mode 'haskell-mode "is" 'shm/case-split)

;; FSharp mode
(require 'fsharp-mode)
(evil-leader/set-key-for-mode 'fsharp-mode "mk" 'fsharp-eval-region)

;; Markup and CSS modes
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'handlebars-mode-hook 'emmet-mode) ;; enable Emmet on Handlebars
(add-hook 'css-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.

;; org mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(evil-leader/set-key-for-mode 'org-mode ">>" 'org-do-demote)
(evil-leader/set-key-for-mode 'org-mode "<<" 'org-do-promote)
(setq org-log-done 'time)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (sh . t)))

;; JavaScript mode
(require 'js2-mode)
(require 'company)
(add-hook 'js-mode-hook 'js2-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook '(lambda () (set-variable 'indent-tabs-mode nil)))
(add-hook 'js2-mode-hook 'skewer-mode)
(evil-leader/set-key;; -for-mode 'js2-mode
  "mk" 'node-run-buffer)

(defun node-run-buffer ()
  (interactive)
  (shell-command "node" (buffer-file-name)))

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
