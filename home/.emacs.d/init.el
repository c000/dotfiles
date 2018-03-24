(let ((path (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file path)
  (ignore-errors
    (load path)))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(load-theme 'wheatgrass)
(show-paren-mode)
(global-linum-mode)
(setq-default indicate-empty-lines t
              indent-tabs-mode nil)
(setq inhibit-splash-screen  t
      require-final-newline t
      scroll-conservatively 4
      scroll-margin 16
      default-tab-width 4
      tab-width 4
      indent-line-function 'indent-to-left-margin
      visible-bell t
      dired-dwim-target t
      split-width-threshold nil)

(if window-system
    (progn
      (global-set-key (kbd "C-x C-c") 'kill-this-buffer)


      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)

      (create-fontset-from-ascii-font
       "DejaVu Sans Mono-12:weight=normal:slant=normal"
       nil
       "DejaVu")

      (set-fontset-font
       "fontset-DejaVu"
       'unicode
       "MyricaM M-12"
       nil
       'append)

      (add-to-list 'default-frame-alist
                   '(font . "fontset-DejaVu"))))

(let ((package 'use-package))
  (unless (package-installed-p package)
    (package-install package)))

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :bind (:map evil-window-map
        ("C-h" . 'evil-window-left)
        ("C-j" . 'evil-window-down)
        ("C-k" . 'evil-window-up)
        ("C-l" . 'evil-window-right)))

(use-package recentf
  :init
  (custom-set-variables
   '(recentf-max-saved-items 1000))
  (recentf-mode 1))

(use-package ivy
  :ensure t
  :defer t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20))

(use-package counsel
  :ensure t
  :defer t
  :init
  (counsel-mode t)
  (setq counsel-find-file-ignore-regexp "~$"))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))

(use-package avy
  :ensure t
  :defer t
  :bind (:map evil-normal-state-map
              ("s" . 'avy-goto-char-timer)))

(use-package swiper
  :ensure t
  :defer t
  :init
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "P")
    (call-interactively (if use-swiper 'swiper 'isearch-forward)))
  :bind (("C-s" . 'isearch-forward-or-swiper)))

(use-package wgrep
  :ensure t
  :defer t
  :init
  (setq wgrep-auto-save-buffer t))

(use-package rg
  :ensure t
  :defer t
  :init
  (add-hook 'rg-mode-hook 'wgrep-ag-setup))

(use-package projectile
  :ensure t
  :defer t)

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode)
  :bind (("M-TAB" . 'company-complete)))

(use-package company-go
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (setq tab-width 4)
                            (setq gofmt-command "goimports")
                            (set (make-local-variable 'company-backends) '(company-go)))))

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs tab-mark))
  (setcar (nthcdr 2 (assq 'tab-mark whitespace-display-mappings)) [?^ ?\t])
  (global-whitespace-mode))

(use-package magit
  :ensure t
  :defer t)

