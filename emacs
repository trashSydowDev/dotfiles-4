;; -*- mode: emacs-lisp -*-
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; -----------------------------------------------------------------------------
;; auto customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#272935" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#f4f1ed"] t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" default)))
 '(inhibit-startup-screen t)
 '(whitespace-display-mappings (quote ((space-mark 32 [46]) (tab-mark 9 [124 45])))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-235"))))
 '(whitespace-tab ((t (:foreground "color-240" :weight bold))))
 '(whitespace-trailing ((t (:background "color-124" :foreground "red" :weight normal)))))
;; -----------------------------------------------------------------------------

(defun require-package (package)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))
  (require package))

(require-package 'ample-theme)
(require-package 'cider)
(require-package 'clojure-mode)
(require-package 'company)
(require-package 'company-ghc)
(require-package 'company-tern)
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
(require-package 'google-this)
(require-package 'haskell-mode)
(require-package 'helm)
(require-package 'js2-mode)
(require-package 'magit)
(require-package 'projectile)
(require-package 'rainbow-delimiters)
(require-package 'rust-mode)
(require-package 'shm)
(require-package 'smart-mode-line)
(require-package 'yasnippet)

;; Basic Settings
(setq ring-bell-function 'ignore)

(require 'exec-path-from-shell) ; load "$PATH" from zsh
(exec-path-from-shell-initialize)

(global-company-mode) ; auto-completion
(define-globalized-minor-mode global-winner-mode winner-mode
  (lambda () (winner-mode 1)))
(global-winner-mode) ; layout changes undo/redo
(cua-mode 1) ; copy/paste/cut commands with reasonable bindings
(electric-pair-mode 1) ; auto-insert matching parentheses/brackets/etc.
(setq-default indent-tabs-mode nil) ; use spaces, not tabs
(ido-mode 1) ; more interactivity

; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Line numbers
(require 'linum)
(global-linum-mode 1) ;; display line numbers
(setq linum-format "%d ")

; Fix crashing on PDF viewing
(add-hook 'doc-view-mode-hook (lambda () (linum-mode -1)))
(defvar is-gui (display-graphic-p) "Whether we're running graphical Emacs")
; Better buffer navigation
(global-set-key (kbd "C-x C-h") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)

;; Evil mode
(require 'evil)
(require 'evil-easymotion)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'evil-surround)
(require 'evil-tabs)
(evil-mode 1)
(global-evil-leader-mode 1)
(global-evil-surround-mode)
(global-evil-tabs-mode 1)
(set-variable 'evil-esc-delay 0) ; Fix escape delays

(evilnc-default-hotkeys)
(evil-leader/set-leader ",")
(evilem-default-keybindings "SPC")
(evil-leader/set-key "e" 'flycheck-list-errors)

; Switch H with ^ and L with $
(define-key evil-motion-state-map "H" 'evil-first-non-blank)
(define-key evil-motion-state-map "L" 'end-of-line)
(evil-leader/set-key "H" 'evil-window-top)
(evil-leader/set-key "L" 'evil-window-bottom)

; Easy tabs switching
(evil-leader/set-key "tt" 'elscreen-create)
(evil-leader/set-key "tn" 'elscreen-next)
(evil-leader/set-key "tp" 'elscreen-previous)

; Better window rotation
(defun window-rotate-downwards-prime ()
  "Swaps a window with its 'cyclic ordering' neighbor"
  :repeat nil
  (let ((current-window (car (window-list)))
        (target-window (cadr (window-list))))
    (if target-window
        (or (swap-window-buffers current-window target-window)
            (select-window target-window)))))

(defun swap-window-buffers (w1 w2)
  "Swaps two windows' buffers."
  (let ((b1 (window-buffer w1))
        (b2 (window-buffer w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)))

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
    ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
  '((space-mark 32 [183] [46])
    ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    (tab-mark 9 [187 9] [9655 9] [92 9])))
(setq whitespace-style '(face tabs trailing tab-mark))
(set-face-attribute 'whitespace-tab nil
                    :background "#f0f0f0"
                    :foreground "#00a8a8"
                    :weight 'bold)
(set-face-attribute 'whitespace-trailing nil
                    :background "#e4eeff"
                    :foreground "#183bc8"
                    :weight 'normal)
(global-whitespace-mode 1)

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

;; Make the interface as bare as possible
(when is-gui (scroll-bar-mode 0))
(menu-bar-mode -1)
(tool-bar-mode -1)

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

;; Haskell mode
(add-hook 'haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(haskell-indentation-ifte-offset 4)
(haskell-indentation-layout-offset 4)
(haskell-indentation-left-offset 4)
(haskell-process-type (quote cabal-repl))

;; JS editing
(add-hook 'js2-mode-hook
          (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook
          '(lambda () (set-variable 'indent-tabs-mode nil)))

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
  `((".*" "~/.emacs.d/auto-save") t) )

(put 'upcase-region 'disabled nil)

(provide '.emacs)
;;; emacs ends here
