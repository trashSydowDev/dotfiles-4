;; -*- mode: emacs-lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272935" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#f4f1ed"])
 '(ansi-term-color-vector
   [unspecified "#272935" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#f4f1ed"] t)
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" default)))
 '(global-whitespace-mode 1)
 '(inhibit-startup-screen t)
 '(whitespace-style
   (quote
    (spaces tabs trailing space-before-tab space-after-tab tab-mark lines lines-trail))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-trailing ((t (:background "gray13" :foreground "#959595" :underline t :weight bold)))))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(cua-mode 1)                        ;; copy/paste/cut commands with reasonable bindings
(electric-pair-mode 1)              ;; auto-insert matching parentheses/brackets/etc.
(global-linum-mode 1)               ;; display line numbers
(setq-default indent-tabs-mode nil) ;; use spaces, not tabs
(ido-mode 1)

;; Fix crashing on PDF viewing
(add-hook 'doc-view-mode-hook
  (lambda () (linum-mode -1)))

(defun is-gui () (display-graphic-p))
(defun is-term () (not (is-gui)))

(defun require-package (package)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))
  (require package))

(require-package 'auto-complete)
(require-package 'ample-theme)
(require-package 'evil)
(require-package 'evil-easymotion)
(require-package 'evil-jumper)
(require-package 'evil-leader)
(require-package 'flycheck)
(require-package 'flycheck-haskell)
(require-package 'exec-path-from-shell)
(require-package 'haskell-mode)
(require-package 'magit)
(require-package 'smart-mode-line)
(require-package 'column-marker)

(load-theme 'ample)
(column-marker-1 80)

(auto-complete-mode 1)
(evilem-default-keybindings "SPC")

(global-evil-leader-mode)
(evil-leader/set-leader ",")

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'js2-mode-hook 'my-disable-indent-tabs-mode)
(defun my-disable-indent-tabs-mode ()
  (set-variable 'indent-tabs-mode nil))

(setq ring-bell-function 'ignore)
(flycheck-mode 1)
(evil-mode 1)
(sml/setup)
(sml/apply-theme 'dark)
(menu-bar-mode -1)
(exec-path-from-shell-initialize)
(tool-bar-mode -1)
(when (is-gui) (scroll-bar-mode 0))

(global-set-key (kbd "C-x C-h") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)

;; el-get
;; ---------------------------------------------------------------------------
(require 'tabbar)
(require 'tabbar-ruler)

(provide '.emacs)

;;; .emacs ends here
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
