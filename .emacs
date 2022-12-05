(setq inihibit-startup-message t)

(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 118)

;; (load-theme 'tango-dark)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages")
			 ("org" . "https//orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; If on non nix platforms
(unless (package-installed-p 'use-package)
  (package-install'use-package))


(require 'use-package)
(setq use-package-always-ensure t)
