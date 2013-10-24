(defvar pre-24 (< emacs-major-version 24))

;;; Set up 'package for emacs 23
(when pre-24
  (load "~/.emacs.d/package.el"))

;;; Slather with elisp
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(;; General
    auto-complete
    color-theme-solarized
    rainbow-delimiters
    undo-tree
    yasnippet

    ;; Clojure
    ac-nrepl
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    nrepl

    ;; Go
    go-mode

    ;; Haskell
    haskell-mode

    ;; Markdown
    markdown-mode

    ;; Project nav
    ack-and-a-half
    projectile)
  "Packages required at launchtime")

(defvar my-24-packages
  '(;; Starter kits
    ;; TODO: see what's actually in these :)
    starter-kit
    starter-kit-bindings
    starter-kit-lisp
    starter-kit-js
    starter-kit-ruby
    powerline)
  "Possibly Emacs 24-specific stuff")

(if (not pre-24)
    (setq my-packages (append my-24-packages my-packages)))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; pre-24 or 24-specific items

(if pre-24

    (progn
      ;; load color-theme and use the old-style theme api
      (load "~/.emacs.d/color-theme.el")
      (require 'color-theme)
      (color-theme-initialize)
      (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
      (require 'color-theme-solarized)
      (color-theme-solarized-dark))
  (progn
    ;;; powerline
    (require 'powerline)
    ;;; solarized
    (require 'solarized-dark-theme)
    (load-theme 'solarized-dark t)))

(defun endarken () (interactive)
  (if pre-24
      (color-theme-solarized-dark)
    (load-theme 'solarized-dark t)))
(global-set-key (kbd "C-c s") 'endarken)

(defun enlighten () (interactive)
  (if pre-24
      (color-theme-solarized-light)
    (load-theme 'solarized-light t)))
(global-set-key (kbd "C-c C-M-s") 'enlighten)

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; ido mode
(require 'ido)
(ido-mode t)


;;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;;; Clojure
; if you want to turn paredit off:
(add-hook 'clojure-mode-hook 'paredit-mode)
;; Clojurescript/EDN highlighting
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
;; nrepl autocomplete
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
(require 'ac-nrepl)
 (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
 (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'nrepl-mode))

;;; org mode!
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; auto-complete
(if (equal nil (boundp 'ac-dictionary-directories))
    (setq ac-dictionary-directories '()))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict" t)
(require 'auto-complete-config)
(ac-config-default)

;;; solarized


;;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'completion-ignored-extensions ".hi")

;;; detab, and tab = 2 spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that, for
;; example.

;;; Line numbering
;;; (from http://www.emacswiki.org/LineNumbers)
(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
     (propertize (format my-linum-format-string line-number) 'face 'linum))
(global-linum-mode 1)

;;; backup files in a backup directory:
(setq backup-directory-alist `(("." . "~/.saved.emacs")))
(setq backup-by-copying t)

;;; Projectile everywhere
(require 'projectile)
(projectile-global-mode)
