(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(electric-pair-mode)

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(if (not (eq ON-MAC nil))
  (set-face-attribute 'default nil :font "FiraCode NF" :height 140)
  (set-face-attribute 'default nil :font "FiraCode NF" :height 120))

(defconst IS-WORK nil)
(defconst WORK-NOTES-PATH "")
(defconst PERSONAL-NOTES-PATH-WINDOWS "d:/Filer/Dokument/Anteckningar/Denote/")
(defconst PERSONAL-JOURNAL-PATH-WINDOWS "d:/Filer/Dokument/Anteckningar/Denote/Journal")
(defconst PERSONAL-NOTES-PATH-UNIX "~/Documents/Denote")
(defconst PERSONAL-JOURNAL-PATH-UNIX "~/Documents/Denote/Journal")
(defun ew/notes-directory ()
    (if (not (eq ON-WINDOWS nil))
        (if (eq IS-WORK t) WORK-NOTES-PATH PERSONAL-NOTES-PATH-WINDOWS) PERSONAL-NOTES-PATH-UNIX))
(defun ew/journal-directory ()
    (if (not (eq ON-WINDOWS nil))
        (if (eq IS-WORK t) WORK-NOTES-PATH PERSONAL-JOURNAL-PATH-WINDOWS) PERSONAL-JOURNAL-PATH-UNIX))

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                             ("org" . "https://orgmode.org/elpa/")
;;                             ("elpa" . "https://elpa.gnu.org/packages/")))
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
            "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(if (eq ON-WINDOWS nil)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))())

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

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
    (setq xref-show-xrew-function #'consult-xref
      xref-show-definitions-function #'consult-xref))
  (use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))
  (use-package consult-project-extra
    :ensure t)

;; Find files with fd instead of find
(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended t)))
    (when re
      (list :command (append
                      (list consult--fd-command
                            "--color=never" "--full-path"
                            (consult--join-regexps re 'extended))
                      opts)
            :highlight hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

(use-package embark
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
    ("C-;" . embark-dwim)        ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :config

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
            '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil
                (window-parameters (mode-line-format . none)))))

(use-package embark-consult
    :after (embark consult)
    :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

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

(load-theme 'doom-oceanic-next t)

(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key))

(setq split-window-preferred-function 'ew/split-window-func)
(defun ew/split-window-func (&optional window)
  (let ((new-window (split-window-sensibly window)))
    (if (not (active-minibuffer-window))
        (select-window new-window))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package general
    :config
      (general-create-definer ew/leader-keys
          :states 'normal
          :keymaps 'override
          :prefix "SPC"
          :global-prefix "SPC")
      (ew/leader-keys
      "b" '(:ignore t :which-key "Buffers")
      "bs" '(consult-buffer :which-key "Switch buffer")
      "d" '(:ignore t :which-key "Denote")
      "dc" '(:ignore :which-key "Create new")
      "dcn" '(denote :which-key "Create new note")
      "dcj" '((lambda () (interactive)(ew/denote-journal)) :which-key "Create new journal entry")
      "df" '((lambda () (interactive)(consult-fd (ew/notes-directory))) :which-key "Find note")
      "dg" '((lambda () (interactive)(consult-ripgrep (ew/notes-directory))) :which-key "Ripgrep notes")
      "E" '(embark-act :which-key "Embark")
      "f" '(:ignore t :which-key "Find")
      "fd" '(consult-dir :which-key "Find directory")
      "fp" '(consult-project-extra-find :which-key "Find all project related entities")
      "fs" '(consult-line :which-key "Find string in file")
      "g" '(:ignore t :which-key "Git")
      "gg" '(magit-status :which-key "Open magit")
      "gb" '(vc-msg-show :which-key "Git Blame")
      "e" '(project-dired :which-key "Toggle dired")
      "h" '(help-command :which-key "Help")
      "l" '(:ignore t :which-key "Lsp")
      "lca" '(eglot-code-actions :which-key "Code Actions")
      "ld" '(eglot-find-declaration :which-key "Find Declaration")
      "lD" '(consult-flymake :which-key "Flymake Diagnostics")
      "lf" '(eglot-format-buffer :which-key "Format Buffer")
      "li" '(eglot-find-implementation :which-key "Find implementation")
      "lr" '(xref-find-references :which-key "Find References")
      "lt" '(eglot-find-typeDefinition :which-key "Find Type Definition")
      "m" '(:ignore t :which-key "Minibuffers")
      "mm" '(popper-toggle-latest :which-key "Toggle Popper")
      "mc" '(popper-cycle :which-key "Cycle Popper buffers")
      "mt" '(popper-toggle-type :which-key "Toggle Popper Types")
      "p" '(:ignore t :which-key "Projects")
      "ps" '(project-switch-project :which-key "Switch to project")
      "S" '(:ignore t :which-key "Snippets")
      "Si" '(yas-insert-snippet :which-key "Insert snippet")
      "s" '(:ignore t :which-key "Splits")
      "sv" '(split-window-right :which-key "Split vertically")
      "sh" '(split-window-below :which-key "Split horizontally")
      "t" '(vterm :which-key "Open new terminal")))

(general-define-key
 :states 'normal
 :keymaps '(override emacs)
 "C-h" 'windmove-left
 "C-l" 'windmove-right
 "C-j" 'windmove-down
 "C-k" 'windmove-up
 "H" 'tab-next
 "L" 'tab-previous
 "gcc" 'evilnc-comment-or-uncomment-lines
 "C-." 'embark-act)

(use-package popper
    :ensure t ; or :straight t
    :init
    (setq popper-reference-buffers
        '("\\*Messages\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            "\\*lsp-log\\*"
            "\\*Warnings\\*"
            "\\*Embark Actions\\*"
            "\\*xref\\*"))
    (popper-mode +1)
    (popper-echo-mode +1))

(use-package hydra)

(use-package rg)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defconst notes-regex "__.*todo.*org$")

(defun ew/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . ew/org-mode-setup)
  :config
  (setq org-ellipsis " ???")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files (directory-files-recursively (ew/notes-directory) notes-regex))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))

(defun ew/get-buffer-file-path ()
   "Get the file path for the currently opened buffer"
   (if (memq (buffer-file-name) '(nil ""))
   "" (abbreviate-file-name (expand-file-name (buffer-file-name)))))

 (defun ew/get-buffer-directory-path ()
   "Get the directory path for the currently opened buffer"
   (if (eq (buffer-file-name) '(nil ""))
   "" (abbreviate-file-name (expand-file-name (file-name-directory (buffer-file-name))))))

 (defun ew/remove-org-agenda-file ()
   "Remove the file corresponding to the currently opened buffer, from the org agenda files"
     (if (not (eq org-agenda-files nil))
     (if (not (eq (memq (ew/get-buffer-file-path) org-agenda-files) nil))
         (setq org-agenda-files (delete (ew/get-buffer-file-path) org-agenda-files)) ()) ()))

 (defun ew/add-org-agenda-files ()
   "Add the file corresponding to the currently opened buffer, to the org agenda files"
     (if (not (memq (buffer-file-name) '(nil "")))
     (if (eq (memq (ew/get-buffer-file-path) org-agenda-files) nil)
         (if (not (eq (string-match-p notes-regex (buffer-file-name)) nil))
             (if (not (eq (string-match-p (ew/notes-directory) (ew/get-buffer-directory-path)) nil))
              (add-to-list 'org-agenda-files (ew/get-buffer-file-path)) ()) ()) ()) ()))

 (defun ew/advice-rename-org-buffer (&rest args)
   (message "Current buffer file name is %s" (ew/get-buffer-file-path))
   (ew/remove-org-agenda-file))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'ew/add-org-agenda-files)))
(advice-add 'rename-file :before 'ew/advice-rename-org-buffer)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet "")
  (org-superstar-headline-bullet-list '("???" "???" "???" "???" "???" "???" "???")))

(use-package org-tempo
  :straight (:host github :repo "luotom/org-tempo")
  :config
  (setq org-tempo-keywords-prefix "!")
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun ew/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ew/org-babel-tangle-config)))

(use-package denote
:config
(setq denote-directory (ew/notes-directory))
(setq denote-templates
      '((journal . "* Vad gjorde jag ig??r?\n\n\n* Vad ska jag g??ra idag?\n\n\n* Finns det n??got som blockerar?\n")))
(setq denote-known-keywords '(note software hardware config education course investigation journal todo)))

(defun ew/denote-journal ()
  (setq-local denote-directory (ew/journal-directory))
  (denote "Standup" '("journal") "org" "" "" 'journal)
  )

(use-package eglot)

(use-package company
  :after eglot
  :hook (eglot-managed-mode . company-mode)
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

(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

(use-package evil-nerd-commenter)

(use-package bufler)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2))
(use-package yaml-mode
  :hook (yaml-mode . eglot-ensure))
(use-package dockerfile-mode
  :hook (dockerfile-mode . eglot-ensure))
(use-package cmake-mode
  :hook (cmake-mode . eglot-ensure))
(use-package go-mode
  :hook (go-mode . eglot-ensure))
(use-package python-mode
  :hook (python-mode . eglot-ensure))
(use-package json-mode
  :hook (json-mode . eglot-ensure))
(use-package fish-mode)
(use-package ansible)
(use-package graphql-mode)
(use-package rust-mode
  :hook (rust-mode . eglot-ensure))
(use-package cargo-mode)
(use-package toml-mode)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(use-package tree-sitter-langs)

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
	    term-mode-hook
	    shell-mode-hook
	    eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package vterm)

(use-package project
  :bind (:map project-prefix-map
            ("t" . project-vterm))
  :preface
  (defun project-vterm ()
      (interactive)
      (defvar vterm-buffer-name)
      (let* ((default-directory (project-root     (project-current t)))
          (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
          (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
          (vterm))))
  :init
  (add-to-list 'project-switch-commands     '(project-vterm "Open Terminal") t)
  (add-to-list 'project-switch-commands     '(project-dired "Open Dired") t)
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode)))

; START TABS CONFIG
;; Create a variable for our preferred tab width
(setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; (OPTIONAL) Shift width for evil-mode users
;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)

;; WARNING: This will change your life
;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

(use-package vc-msg)
