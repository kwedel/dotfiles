(require 'package)

(eval-when-compile 'use-package)
(require 'use-package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(ido-mode t)
(tool-bar-mode 0)
(global-display-line-numbers-mode 1)
(recentf-mode 1) ; Recent files
(save-place-mode 1) ; Remember position in files
(global-auto-revert-mode 1) ; Automatically revert files when changed on disk

;; Undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind (("C-x C-u" . undo-tree-visualize)
	 :map undo-tree-map
	 ("C-x u" . undo-tree-undo)
	 )
  )

;; Smerge
(setq smerge-command-prefix "\C-cv")

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-dispatch)
(global-set-key (kbd "C-c f") 'magit-file-dispatch)

;; Elpy
(elpy-enable)
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")
;; Auto format code on save
(add-hook 'elpy-mode-hook (lambda ()
			    (add-hook 'before-save-hook
				      'elpy-black-fix-code nil t)))

;; GPTel
(use-package gptel
  :ensure t
  :config
  (setq
   gptel-model 'qwen2.5-coder:14b-instruct-q2_K
   gptel-backend (gptel-make-ollama "Ollama"
                                      :host "localhost:11434"
                                      :stream t
                                      :models '(qwen2.5-coder:14b-instruct-q2_K llama3.2:latest)))
  :bind ("C-c <RET>" . gptel-send))
(global-set-key (kbd "C-c RET") 'gptel-send)



;; Dockerfile-mode
(require 'dockerfile-mode)

;; Projectile
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Mac-specific settings
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta 0
      mac-command-modifier 'meta
      mac-option-modifier 'none
      mac-right-command-modifier 'super)

 ;; Org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; (add-hook 'org-mode 'org-indent-mode)
(with-eval-after-load 'org       
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook 'visual-line-mode))
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Setup agenda
(setq org-agenda-files '("~/Dropbox/Org/inbox.org"
                         "~/Dropbox/Org/gtd.org"
                         "~/Dropbox/Org/tickler.org"))
(setq org-agenda-files '("/mnt/c/Users/SG4L/OneDrive - Københavns Kommune/org/todo.org"))

;; Setup capture
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file "~/Dropbox/Org/inbox.org")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/Org/tickler.org" "Tickler")
                               "* %i%? \n %U")))

;; Setup refile
;(setq org-completion-use-ido t)
;(setq org-outline-path-complete-in-steps t)
(setq org-refile-use-outline-path 1)
(setq org-refile-targets '(("~/Dropbox/Org/gtd.org" :maxlevel . 2)
                           ("~/Dropbox/Org/someday.org" :maxlevel . 2)
			   ("~/Dropbox/Org/refs.org" :maxlevel . 3)
                           ("~/Dropbox/Org/tickler.org" :maxlevel . 1)))

;; Other org
(setq org-agenda-text-search-extra-files '(agenda-archives)) ; Search archive
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item)))) ; Blank lines


;; Visual-regexp-steroids
(require 'visual-regexp-steroids)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Multiple cursors
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c e") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-c n") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-S-c p") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-S-c a") 'mc/mark-all-like-this)

;; Easy-repeat
;; (require 'easy-repeat)

;; Smex
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Easy-kill
;; (global-set-key [remap kill-ring-save] 'easy-kill)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(tango-dark))
 '(easy-repeat-command-list
   '(other-window org-previous-visible-heading mc/mark-next-like-this mc/mark-previous-like-this))
 '(elpy-rpc-python-command "python3")
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (python . "You are a large language model and a careful Python programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (chat . "You are a large language model and a conversation partner. Respond concisely.")))
 '(org-babel-load-languages '((python . t) (emacs-lisp . t)))
 '(package-selected-packages
   '(typescript-mode toml-mode company undo-tree vundo gptel restclient flymake-ruff pip-requirements tide dockerfile-mode projectile visual-regexp visual-regexp-steroids writeroom-mode markdown-mode elpy yaml-mode use-package easy-repeat easy-kill multiple-cursors magit web-mode smex helm auctex))
 '(vr/command-python
   "python3 /home/sg4l/.emacs.d/elpa/visual-regexp-steroids-20170222.253/regexp.py")
 '(vr/engine 'python))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
