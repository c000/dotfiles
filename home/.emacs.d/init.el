(load-theme 'wheatgrass)
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

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-keys "abcdefghijklmnopqrstuvwxyz")
 '(company-search-regexp-function (quote company-search-flex-regexp))
 '(company-selection-wrap-around t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (yafolding avy magit counsel-projectile projectile flycheck wgrep rg ace-jump-mode company counsel evil)))
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(let '(my/packages '(
                     ace-jump-mode
                     company
                     counsel
                     evil
                     flycheck
                     rg
                     wgrep
                     company-go
                     projectile
                     counsel-projectile
                     magit
                     avy
                     ))
  (dolist (package my/packages)
    (unless (package-installed-p package)
      (package-install package))))

(require 'recentf)
(recentf-mode 1)

(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-timer)
(define-key evil-window-map "\C-h" 'evil-window-left)
(define-key evil-window-map "\C-j" 'evil-window-down)
(define-key evil-window-map "\C-k" 'evil-window-up)
(define-key evil-window-map "\C-l" 'evil-window-right)

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

(require 'ivy)
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 20)

(counsel-mode t)
(setq counsel-find-file-ignore-regexp "~$")
(defun isearch-forward-or-swiper (use-swiper)
  (interactive "P")
  (call-interactively (if use-swiper 'swiper 'isearch-forward)))
(global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
(global-set-key (kbd "M-TAB") 'company-complete)

(require 'rg)
(add-hook 'rg-mode-hook 'wgrep-ag-setup)
(setq wgrep-auto-save-buffer t)


(require 'company)
(global-company-mode)

(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark))
(setcar (nthcdr 2 (assq 'tab-mark whitespace-display-mappings)) [?^ ?\t])
(global-whitespace-mode)

(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(require 'go-mode)
(require 'company-go)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda ()
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (setq tab-width 4)
                          (setq gofmt-command "goimports")
                          (set (make-local-variable 'company-backends) '(company-go))))
