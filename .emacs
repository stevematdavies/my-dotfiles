;; Stop checking the docs on this file
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
;; -----------------------------------------------------------------

;; Gloals for font
(defvar efs/default-font-size 116)
(defvar efs/default-variable-font-size 116)

;; Clear the clutter
(setq inhibit-startup-message t)
(setq global-visual-line-mode t)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell t)
;; -----------------------------------------------------------------

;; add a place for extra themes
(add-to-list 'custom-theme-load-path
	     (file-name-as-directory "~/.emacs.d/themes"))
;; -----------------------------------------------------------------

;; Some Editor sytling
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)
;; -----------------------------------------------------------------

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size)
;; -----------------------------------------------------------------

;; Better meta
(column-number-mode) ;; set column numbers
(global-display-line-numbers-mode t)
;; -----------------------------------------------------------------

;; ensure buffer droppings go to tempfile
 (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
;; -----------------------------------------------------------------

;;  configures emacs so that files deleted via emacs are moved to the recycle bin
(setq delete-by-moving-to-trash t)
;; -----------------------------------------------------------------

;; Makesure we use good 'ol Firefox for browser links!
(setq browse-url-browser-function 'browse-url-firefox)
;; -----------------------------------------------------------------

;; Set line numbers, but only for programming modes
; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	              treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
  ;; -----------------------------------------------------------------

;; Some global keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; -----------------------------------------------------------------

;; Setting up 'use-package' for better package installing
(require 'package)

;; > Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; > incase of non nix platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; -----------------------------------------------------------------

;; Nice simple theme
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))


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
	 ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1))
;; -----------------------------------------------------------------

;; ivy rich to sweeten ivy >>
(use-package ivy-rich
  :config
  (setq ivy-rich-original-display-transformers-list nil)
  :init
  (ivy-rich-mode 1))
;; -----------------------------------------------------------------

;; Rainbow delims!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; -----------------------------------------------------------------

;; Which key to help selections
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))
;; -----------------------------------------------------------------

;; help on superpowers
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
  ;; -----------------------------------------------------------------

;; general
(use-package general)
;; -----------------------------------------------------------------

;; all-the-icons for a clean status bar
(use-package all-the-icons)
;; -----------------------------------------------------------------

;; Emmet Magic
(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq-default emmet-move-cursor-between-quote t))
;; -----------------------------------------------------------------
  (use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))
;; -----------------------------------------------------------------

;; For a cleaner status bar look
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
;; -----------------------------------------------------------------

;; web-mode of course
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
;; -----------------------------------------------------------------

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))
;; -----------------------------------------------------------------

;; projectile and counsel
(use-package counsel-projectile
  :config (counsel-projectile-mode))
;; -----------------------------------------------------------------

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; -----------------------------------------------------------------

;; Run a simple http server when needed
(use-package simple-httpd
  :ensure t)
;; -----------------------------------------------------------------

;; The one and only ORG mode!
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; > Org!
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

;; > cleaner lists
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; > some filler
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; > add some snippet love
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; > This is needed as of Org 9.2
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
;; -----------------------------------------------------------------

;; Super charge org-
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
	  :config
  (org-roam-setup))
;; -----------------------------------------------------------------

;; Flycheck, keep in line!
(use-package flycheck
  :ensure t
  :hook ((after-init . global-flycheck-mode)))
;; -----------------------------------------------------------------

;; set up breadcrumbs for files
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))
;; -----------------------------------------------------------------

;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; > add some better looking popups
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-position 'bottom))
;; -----------------------------------------------------------------

;; Treemacs intergration
(use-package lsp-treemacs
  :after lsp)
;; -----------------------------------------------------------------

;; > Ivy integration
(use-package lsp-ivy)
;; -----------------------------------------------------------------

;; Company
(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	  ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
	  ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
;; -----------------------------------------------------------------

(use-package company-box
  :hook (company-mode . company-box-mode))
;; -----------------------------------------------------------------

;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))
;; -----------------------------------------------------------------
