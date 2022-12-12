;; Stop checking the docs on this file
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
;; -----------------------------------------------------------------

;; Gloals for font
(defvar efs/default-font-size 130)
(defvar efs/default-variable-font-size 130)

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


;; Get Scandi keys working properly
(set-input-mode (car (current-input-mode))
		(nth 1 (current-input-mode))
		'accept-8bit-input)

;; ------------------------------------------------------------------INPUT)



;; add a place for extra themes
(add-to-list 'custom-theme-load-path
	     (file-name-as-directory "~/.emacs.d/themes"))
;; -----------------------------------------------------------------

;; Some Editor sytling
(set-face-attribute 'default nil :font "Fira Mono" :height efs/default-font-size)
;; -----------------------------------------------------------------

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Mono" :height efs/default-font-size)
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

;; Set line numbers, but only for programming modes
; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
		cider-repl-hook
		cider-repl
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
  ;; -----------------------------------------------------------------

;; Some global keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; -------------------------------


;; Clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;; add a place for extra themes
(add-to-list 'custom-theme-load-path
	     (file-name-as-directory "~/.emacs.d/themes"))
;; -----------------------------------------------------------------

;; Some Editor sytling
(set-face-attribute 'default nil :font "Fira Mono" :height efs/default-font-size)
;; -----------------------------------------------------------------

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Mono" :height efs/default-font-size)
;; -----------------------------------------------------------------

;; Variable pitch
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-font-size :weight 'regular)
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

;; Set line numbers, but only for programming modes
; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
		cder-repl-hook
		cider-repl
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
  ;; -----------------------------------------------------------------

;; Some global keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; -----------------------------------------------------------------


;; Firefox For Links
(setq Browse-Url-Browser-Function #'Browse-Url-Firefox)

;; Setting Up 'Use-Package' For Better Package Installing
(require 'package)

;; > Repositories
(setq Package-archives '(("melpa" . "https://melpa.org/packages/")
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

;; Lets stop that pesky mouse!
(use-package disable-mouse)
(global-disable-mouse-mode nil)


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
(use-package all-the-icons
  :if (display-graphic-p))
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
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil))


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
  (org-bullets-bullet-list '("◉" "○" "●" "◆" "✚" "✿" "✸" )))


;; Replace list-hyphen with a dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\)"
			   (0 (prog1 () (compose-region (match-beginning 1)(match-end 1) "•"))))))




;; Heading sizes in org mode
(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))



;; Adjust pitch font display elements
;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

    ;; Get rid of the background on column views
    (set-face-attribute 'org-column nil :background nil)
    (set-face-attribute 'org-column-title nil :background nil)

;; -----------------------------------------------------------------


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
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position bottom)
  (lsp-ui-doc-show))
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
(put 'upcase-region 'disabled nil)

;; Easier commenting
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
;; -----------------------------------------------------------------


;; Clojure and Cider setup
(use-package cider
  :ensure t
  :config
  (setq cider-repl-result-prefix "=> "
	cider-eval-result-prefix ""
	cider-connection-message-fn nil
	cider-repl-prompt-function #'opts/cider-repl-prompt
	cider-use-overlays nil
	cider-repl-display-help-banner nil))

(defun opts/cider-repl-prompt (namespace)
   "Return a prompt string that mentions NAMESPACE."
   (format "%s " (cider-abbreviate-ns namespace)))

(use-package clojure-mode
  :ensure t
  :config (global-prettify-symbols-mode t)
  :init (defconst clojure--prettify-symbols-alist
	   '(("fn" . ?λ)
	     ("__" . ?⁈))))

(use-package color-identifiers-mode
  :hook (clojure-mode . color-identifiers-mode))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))

;; -----------------------------------------------------------------

