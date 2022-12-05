(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell t)

(set-face-attribute 'default nil :font "Source Code Pro" :height 116)
(column-number-mode) ;; Set column numbers
(global-display-line-numbers-mode t)


;; Set line numbers, but only for programming modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode
		eshell-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; Some global keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

	
;; Ivy Setup
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; Ivy rich to sweeten ivy >>
(use-package ivy-rich
  :config
  (setq ivy-rich-original-display-transformers-list nil)
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-monokai-machine t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Rainbow delims!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Help on superpowers
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helplful-key))

(use-package general)
;; Icons!!
(use-package all-the-icons)

(use-package emmet-mode
  :ensure t)

(use-package web-mode :ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
	
	
;; Ivy Setup
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; Ivy rich to sweeten ivy >>
(use-package ivy-rich
  :config
  (setq ivy-rich-original-display-transformers-list nil)
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-monokai-machine t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Rainbow delims!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Help on superpowers
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helplful-key))

(use-package general)
;; Icons!!
(use-package all-the-icons)

(use-package emmet-mode
  :ensure t)

(use-package web-mode :ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
	
	
;; Ivy Setup
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; Ivy rich to sweeten ivy >>
(use-package ivy-rich
  :config
  (setq ivy-rich-original-display-transformers-list nil)
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-monokai-machine t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Rainbow delims!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Help on superpowers
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helplful-key))

(use-package general)
;; Icons!!
(use-package all-the-icons)

(use-package emmet-mode
  :ensure t)

(use-package web-mode :ensure t)
(add-to-list 'auto-mode-alist '(".phtml'" . web-mode))
(add-to-list 'auto-mode-alist '(".tpl.php'" . web-mode))
(add-to-list 'auto-mode-alist '(".erb'" . web-mode))
(add-to-list 'auto-mode-alist '(".mustache'" . web-mode))
(add-to-list 'auto-mode-alist '(".html'" . web-mode))
(add-to-list 'auto-mode-alist '(".ctp'" . web-mode))
(add-to-list 'auto-mode-alist '(".php'" . web-mode))
(add-to-list 'auto-mode-alist '(".js'" . web-mode))
(add-to-list 'auto-mode-alist '(".jsx'" . web-mode))
(add-to-list 'auto-mode-alist '(".css'" . web-mode))
(add-to-list 'auto-mode-alist '(".scss'" . web-mode))
(add-to-list 'auto-mode-alist '(".xml'" . web-mode))
(add-to-list 'auto-mode-alist '(".ts'" .web.mode))
(add-to-list 'auto-mode-alist '(".tsx'" .web.mode))
(add-hook 'web-mode-hook 'emmet-mode)


;; Projectile
(use-package projectile
  :ensure t
  :pin melpa-stable
  :custom ((projectile-completion-system 'ivy))
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))



