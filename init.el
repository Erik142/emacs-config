;; Basic settings
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)

(set-face-attribute 'default nil :font "FiraCode NF" :height 120)

(load-theme 'doom-gruvbox t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Packages
(use-package command-log-mode)
(use-package counsel
	:bind (("M-x" . counsel-M-x)
				 ("C-x b" . counsel-ibuffer)
				 ("C-x C-f" . counsel-find-file)
				 :map minibuffer-local-map
				 ("C-r" . counsel-minibuffer-history)))
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
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
	:custom ((doom-modeline-height 30)))
(use-package doom-themes)
(use-package all-the-icons
  :if (display-graphic-p)
	:ensure t)
(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))
(use-package which-key
	:init (which-key-mode)
	:diminish which-key-mode
	:config
	(setq which-key-idle-delay 0))
(use-package ivy-rich
	:init
	(ivy-rich-mode 1))
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package general
	:config
	(general-create-definer ew/leader-keys
		:keymaps '(normal insert visual emacs)
		:prefix "SPC"
		:global-prefix "C-SPC"))
(use-package key-chord)
;;Exit insert mode by pressing j and k quickly
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)
(use-package hydra)
(use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    (when (file-directory-p "~/Projekt")
	    (setq projectile-project-search-path '("~/Projekt")))
    (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
  :config (counsel-projectile-mode))
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
								term-mode-hook
								shell-mode-hook
								eshell-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0))))
