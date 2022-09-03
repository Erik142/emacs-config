;; Basic settings
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)

(set-face-attribute 'default nil :font "FiraCode NF" :height 120)
(electric-pair-mode)

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

(setq split-window-preferred-function 'ew/split-window-func)
(defun ew/split-window-func (&optional window)
  (let ((new-window (split-window-sensibly window)))
    (if (not (active-minibuffer-window))
        (select-window new-window))))

;; Packages
(use-package vertico
  :init
  (vertico-mode))
(use-package savehist
  :init
  (savehist-mode))
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref))
(use-package consult-dir
:bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
(use-package consult-project-extra
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
	:custom ((doom-modeline-height 30)))
(use-package doom-themes)

(load-theme 'doom-gruvbox t)

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
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key))
  
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

(use-package key-chord)
;;Exit insert mode by pressing j and k quickly
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)
(use-package hydra)
(use-package rg)
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package org)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  ;;(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package popper
  :ensure t ; or :straight t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*lsp-log\\*"
          "\\*Warnings\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package general
    :config
    (general-create-definer ew/leader-keys
        :states '(normal treemacs)
	:keymaps 'override
	:prefix "SPC"
	:global-prefix "C-SPC")
    (ew/leader-keys
      "b" '(:ignore t :which-key "Buffers")
      "bs" '(consult-buffer :which-key "Switch buffer")
      "f" '(:ignore t :which-key "Find")
      "fd" '(consult-dir :which-key "Find directory")
      "fp" '(consult-project-extra-find :which-key "Find all project related entities")
      "g" '(:ignore t :which-key "Git")
      "gg" '(magit-status :which-key "Open magit")
      "e" '(project-dired :which-key "Toggle dired")
      "h" '(help-command :which-key "Help")
      "m" '(:ignore t :which-key "Minibuffers")
      "mm" '(popper-toggle-latest :which-key "Toggle Popper")
      "mc" '(popper-cycle :which-key "Cycle Popper buffers")
      "mt" '(popper-toggle-type :which-key "Toggle Popper Types")
      "p" '(:ignore t :which-key "Projects")
      "pS" '(ew/search-projects :which-key "Scan for new projects")
      "ps" '(project-switch-project :which-key "Switch to project")
      "s" '(:ignore t :which-key "Splits")
      "sv" '(split-window-right :which-key "Split vertically")
      "sh" '(split-window-below :which-key "Split horizontally")))

(setq ew/search-project-dirs '("~/Projekt"))
(setq ew/search-project-qualifiers '(".git"))

(defun ew/search-projects ()
    (message "Hello from my search function!"))

(use-package bufler)

(general-define-key
 :states '(normal treemacs)
 :keymaps '(override emacs treemacs)
 "C-h" 'windmove-left
 "C-l" 'windmove-right
 "C-j" 'windmove-down
 "C-k" 'windmove-up
 "H" 'tab-next
 "L" 'tab-previous
 "gcc" 'evilnc-comment-or-uncomment-lines)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package evil-nerd-commenter)

;; Language modes
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))



;; Line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
	    term-mode-hook
	    shell-mode-hook
	    eshell-mode-hook
	    treemacs-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(project-tab-groups bufler sideline-blame evil-nerd-commenter lsp-java company-box company typescript-mode centaur-tabs general popper lsp-ui lsp-mode magit rg hydra key-chord evil-collection evil helpful which-key rainbow-delimiters all-the-icons doom-themes doom-modeline consult-project-extra consult-dir consult marginalia orderless vertico use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
