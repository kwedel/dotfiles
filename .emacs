(require 'package)
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

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

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

;;(add-hook 'org-mode 'org-indent-mode)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Setup agenda
(setq org-agenda-files '("~/Dropbox/Org/inbox.org"
                         "~/Dropbox/Org/gtd.org"
                         "~/Dropbox/Org/tickler.org"))

;; Setup capture
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file "~/Dropbox/Org/inbox.org")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/Org/tickler.org" "Tickler")
                               "* %i%? \n %U")))

;; Setup refile
(setq org-refile-targets '(("~/Dropbox/Org/gtd.org" :maxlevel . 1)
                           ("~/Dropbox/Org/someday.org" :level . 2)
                           ("~/Dropbox/Org/tickler.org" :maxlevel . 1)))

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-m e") 'mc/edit-lines)
(global-set-key (kbd "C-S-m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-m a") 'mc/mark-all-like-this)

;; Easy-repeat
(require 'easy-repeat)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(easy-repeat-command-list
   (quote
    (other-window org-previous-visible-heading org-next-visible-heading org-forward-heading-same-level org-backward-heading-same-level outline-up-heading outline-next-visible-heading outline-previous-visible-heading outline-forward-same-level outline-backward-same-level git-gutter:previous-hunk git-gutter:next-hunk mc/mark-next-like-this mc/mark-previous-like-this)))
 '(package-selected-packages
   (quote
    (easy-repeat easy-kill multiple-cursors magit web-mode smex helm auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
