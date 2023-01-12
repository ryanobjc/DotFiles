(setq custom-file "~/.emacs-29.d/custom.el")
(load custom-file)

;;; Bootstrap straight.el -- should only happen on new installs.
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

;;; Install packages (but not 'use' them, see below for use-package)
(straight-use-package
 '(copilot :type git :host github
           :repo "zerolfx/copilot.el"
           :files ("dist" "*.el")))
(mapc #'straight-use-package
      '(znc
        f
        projectile
        scala-mode
        slime
        yaml-mode
        kotlin-mode
        yasnippet
        magit
        password-store
        code-review
        lsp-mode
        lsp-ui
        framemove
        rustic
        kubernetes
        dap-mode
        dotenv-mode))
(straight-use-package 'use-package)


;;; Use and configure packages (use-package)

(put 'narrow-to-region 'disabled nil)
(add-to-list 'exec-path "/opt/homebrew/bin")
;; not necessary because the zsh/profile/something gets execute on mac login
;;(add-to-list 'exec-path "~/.cargo/bin/")
(add-to-list 'load-path "~/.emacs-29.d/lisp")
(add-to-list 'package-archives '("tromney" . "http://tromey.com/elpa/"))
;;; Emacs 29+
;;; This directory was created by first cloning:
;;; git@github.com:casouri/tree-sitter-module.git
;;; running batch.sh and then copying the resulting so files into
;;; the following directory!
(add-to-list 'treesit-extra-load-path "~/.emacs-tree-sitter")


;;; wind move -- this is built in to emacs
(use-package windmove
  :config
  (windmove-default-keybindings 'super))
;;; Frame move, this is NOT built into emacs
(use-package framemove
  :config
  (setq framemove-hook-into-windmove t)
  :after windmove)

;;; Transparent descryption/encryption of file
(use-package epa-file
  :init (epa-file-enable))


(use-package znc
  :config

  (defun ryan-znc ()
    (interactive)
    (erc-tls
     :server "boopity bo"
     :port 6431
     :nick "ryan42"
     :user "ryan@erc"
     :full-name "RR"
     :password (password-store-get "ryan-znc"))))
   
(use-package mu4e
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")

(use-package ryan-mu4e
  :after mu4e)

(use-package smtpmail
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        (expand-file-name "~/.authinfo.gpg")
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))

  


;;
;; Basics
;;
(use-package better-defaults
   )
(use-package magit
  :bind (("C-c m" . magit)))
(use-package code-review
  :defer)
(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list))
  :init (setq helm-split-window-inside-p nil)
  :init (helm-mode 1))
(use-package s)
(use-package f)

;;; Add xterm-256color support to comint and more
(use-package xterm-color
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (setq python-shell-font-lock-enable nil)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (font-lock-mode -1)
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (setq comint-output-filter-functions
                    (remove 'ansi-color-process-output comint-output-filter-functions))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package solidity-mode
  :defer)
(require 'cl) ;; just for solidity flycheck which needs to be fixed
(use-package solidity-flycheck
  :defer)


;; slime and common lisp
(use-package slime
  :defer
  :init
  (setq inferior-lisp-program "sbcl")
  )

;; eglot

(use-package pyvenv
  )

;;
;; Basic Code tools
;;

(use-package yaml-mode
  :defer)

(use-package solidity-flycheck)
;;(require 'solidity-flycheck)
(use-package flycheck
  :init (global-flycheck-mode))
(use-package flycheck-inline
  :after (flycheck)
  :hook (flycheck-mode . flycheck-inline-mode))

;(use-package project)

(use-package protobuf-mode
  :defer)
(use-package markdown-mode
  :defer)

;;
;; borrowed from https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/;;

(use-package which-key
  :init (which-key-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package dotenv-mode
  :after projectile
  :config
  (defun dotenv-projectile-hook ()
    "Projectile hook."
    (minibuffer-message "Switching ENV to project at %s" (projectile-project-root))
    (dotenv-update-project-env (projectile-project-root)))
  
  (add-to-list 'projectile-after-switch-project-hook #'dotenv-projectile-hook))


;; Not necessary on emacs-29+ which I am using.
;; (use-package tree-sitter
;;   :commands (tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :config
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;;                '(typescript-tsx-mode . tsx)))

;; (use-package typescript-mode
;;   :mode "\.ts\'"
;;   :config (setq typescript-indent-level 2)
;;   :init
;;   (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[tsx]")
;;   (add-to-list 'auto-mode-alist '("\.tsx\'" . typescript-tsx-mode))
;;   (add-hook 'typescript-tsx-mode-hook
;;             (lambda () (tree-sitter-mode) (tree-sitter-hl-mode))) )






;;
;; RUST setup
;; 

;; Rustic depends on lsp-mode, so install it first (:ensure method)
(use-package lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "cargo-clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))

  :hook
  ((python-mode . lsp)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


(use-package rustic
  :commands rustic-mode
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))


(use-package ledger-mode
  :commands ledger-mode)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Auto completion and code snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :config
  (delq 'company-preview-if-just-one-frontend company-frontends)
  :hook (python-mode . company-mode)
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(use-package company-terraform)

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :if (executable-find "lldb-mi")
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         ;; uncomment if lldb-mi is not in PATH
         ;; :lldbmipath "path/to/lldb-mi"
         ))))




;;(use-package flycheck-rust
;;  :after (rust-mode flycheck)   ;; because of the flycheck-rust-setup
;;  :hook (flycheck-mode . flycheck-rust-setup))
;; maybe consider ace-window?
;; (use-package ace-window
;;              :bind (("C-x o" . ace-window))
;;              :init (setq aw-keys (?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;; (use-package expand-region
;;              :bind (("C-@" . er/expand-region)))
;;; rust stuff ;;;;;
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config (setq org-log-done 'time))
;; (use-package rust-mode
;;   :bind (:map rust-mode-map ([tab] . company-indent-or-complete-common))
;;   :ensure t)
;;(use-package eglot
;;  :ensure t)
;(use-package racer
;;  :ensure t
;;  :hook (rust-mode . racer-mode)
;;         )



;;(use-package eldoc
;;  :ensure t
;;  :hook (racer-mode . eldoc-mode))
;;; :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(use-package copilot
  :hook (prog-mode yaml-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)))

(use-package ryan-order)

(use-package ryan-znc)

(use-package password-store)

(provide 'init)


;;; init.el ends here.
