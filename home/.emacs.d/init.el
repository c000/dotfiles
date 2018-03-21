(if window-system (progn
		    (global-set-key (kbd "C-x C-c") 'kill-this-buffer)

		    (load-theme 'tango-dark)

		    (menu-bar-mode 0)
		    (tool-bar-mode 0)
		    (scroll-bar-mode 0)

		    (create-fontset-from-ascii-font
		    "DejaVu Sans Mono-9"
		    nil
		    "DejaVu")

		    (add-to-list 'default-frame-alist
				'(font . "fontset-DejaVu"))
))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company counsel evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'evil)
(evil-mode 1)

(show-paren-mode)
(global-linum-mode)
(setq-default indicate-empty-lines t)
(setq scroll-conservatively 4
      scroll-margin 16)


(require 'ivy)
(ivy-mode t)
(setq ivy-height 40)

(counsel-mode t)

(require 'company)
(global-company-mode)
