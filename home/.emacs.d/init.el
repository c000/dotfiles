(let ((path (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file path)
  (ignore-errors
    (load path)))
(require 'package)
(add-to-list 'package-archives
             '("mc/mark-next-like-this-wordmelpa" . "http://melpa.org/packages/"))
(package-initialize)

(load-theme 'wombat)
(show-paren-mode)
;; (global-linum-mode)
(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              truncate-lines t)
(setq inhibit-splash-screen  t
      require-final-newline t
      scroll-conservatively 4
      scroll-margin 16
      default-tab-width 2
      indent-line-function 'indent-to-left-margin
      visible-bell t
      dired-dwim-target t
      split-width-threshold nil
      max-specpdl-size 32000
      compilation-window-height 20)

;; C-h to BackSpace
(define-key key-translation-map [?\C-h] [?\C-?])

(defun my-compilation-hook()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))

(if window-system
    (progn
      (global-set-key (kbd "C-x C-c") 'kill-this-buffer)


      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)

      (create-fontset-from-ascii-font
       "DejaVu Sans Mono-11:weight=normal:slant=normal"
       nil
       "DejaVu")

      (set-fontset-font
       "fontset-DejaVu"
       'unicode
       "MyricaM M-11"
       nil
       'append)

      (add-to-list 'default-frame-alist
                   '(font . "fontset-DejaVu"))))

(let ((package 'use-package))
  (unless (package-installed-p package)
    (package-install package)))

(use-package evil
  :ensure t
  :defer t
  :init
  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for evil leader")
  (define-key my-leader-map "l" 'imenu-list-smart-toggle)
  (define-key my-leader-map "w" 'toggle-truncate-lines)
  (define-key my-leader-map "s" 'swiper)
  (evil-mode 1)
  (defun evil-compilation-mode-hook ()
    (define-key compilation-mode-map (kbd "g") nil)
    (define-key compilation-mode-map (kbd "h") nil)
    (define-key compilation-mode-map (kbd "SPC") my-leader-map))
  (add-hook 'compilation-mode-hook 'evil-compilation-mode-hook)
  :bind (("<escape>" . evil-normal-state)
         ("M-p" . 'previous-error)
         ("M-n" . 'next-error)
         :map evil-window-map
         ("C-h" . 'evil-window-left)
         ("C-j" . 'evil-window-down)
         ("C-k" . 'evil-window-up)
         ("C-l" . 'evil-window-right))
  :config
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)
  (custom-set-variables
   '(evil-flash-delay 180)
   '(evil-move-beyond-eol t)
   '(evil-auto-balance-windows nil)
   '(dabbrev-case-replace nil)))

(use-package which-key
  :ensure t
  :defer t
  :hook (after-nit . which-key-mode))

(use-package undo-tree
  :init
  (custom-set-variables
   '(undo-tree-auto-save-history t)
   '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))

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
  :defer t
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (if (not window-system)
      (diff-hl-margin-mode 1))
  :config
  (diff-hl-flydiff-mode t))

(use-package avy
  :ensure t
  :defer t
  :bind (:map my-leader-map
              ("f" . 'avy-goto-char-timer))
  :config
  (custom-set-variables
   '(avy-timeout-seconds 2.0)
   '(avy-keys (number-sequence ?a ?z))))

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

(use-package wgrep-ag
  :ensure t
  :defer t)

(use-package rg
  :ensure t
  :defer t
  :init
  (add-hook 'rg-mode-hook 'wgrep-ag-setup))

(use-package projectile
  :ensure t
  :defer t
  :bind (("<f5>" . projectile-test-project)
         ("<f8>" . projectile-compile-project)))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode)
  :bind (:map my-leader-map
              ("r" . 'counsel-projectile-rg)))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  :hook (go-mode . yas-minor-mode))

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode)
  :bind (("M-TAB" . 'company-complete)
         :map evil-insert-state-map
         ("C-o" . 'company-complete)
         :map company-active-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous)
         :map company-search-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous)))

(use-package flycheck
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :defer t)

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :bind (:map my-leader-map
              ("d" . 'lsp-ui-doc-mode)
              ("p" . 'lsp-ui-peek-find-references))
  :config
  (custom-set-variables
   '(lsp-ui-sideline-enable nil)))

(use-package company-lsp
  :after (company)
  :ensure t
  :defer t
  :commands company-lsp
  :init
  (push 'company-lsp company-backends))

(use-package go-mode
  :ensure t
  :defer t
  :hook (go-mode . lsp)
  :init
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (setq tab-width 2)
                            (setq gofmt-command "goimports")
                            )))

(use-package go-impl
  :ensure t
  :defer t
  :functions go-impl)

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq whitespace-display-mappings '((tab-mark ?\t [?^ ?\t])))
  ;; (setcar (nthcdr 2 (assq 'tab-mark whitespace-display-mappings)) [?^ ?\t])
  (global-whitespace-mode))

(use-package magit
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  (defun haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))
  :config
  (custom-set-variables
   '(haskell-stylish-on-save t)
   '(haskell-indentation-left-offset 4)
   '(haskell-indentation-starter-offset 4)
   '(haskell-indentation-where-pre-offset -2)
   '(haskell-indentation-where-post-offset 0))
  :hook
  (haskell-mode . (lambda ()
                    haskell-indentation-mode
                    (evil-local-set-key 'normal (kbd "o") 'haskell-evil-open-below)
                    (evil-local-set-key 'insert (kbd "RET") 'haskell-indentation-newline-and-indent))))

(use-package intero
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package symbol-overlay
  :ensure t
  :defer t
  :bind (:map my-leader-map
              (":" . 'symbol-overlay-put)
              :map symbol-overlay-map
              ("N" . 'symbol-overlay-jump-prev)
              ("p" . nil)
              ("e" . nil)
              ("d" . nil)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-d" . 'mc/mark-next-like-this-word)))

(use-package ialign
  :ensure t
  :bind (("C-x l" . #'ialign)))

(use-package imenu-list
  :ensure t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package typescript-mode
  :ensure t
  :defer t
  :hook (typescript-mode . lsp))

(use-package origami
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t)

(put 'narrow-to-region 'disabled nil)
