(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name "custom-load-files" "~/.emacs.d/"))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'use-package)
(require 'diminish)
(require 'bind-key)

(setq load-prefer-newer t)

(setq user-mail-address "swananddhawan@gmail.com")
(setq user-full-name "Swanand Dhawan")

;; Clean GUI
(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(load "server")
(unless (server-running-p) (server-start))

;;----------------------------------------------------------------------------
;; Mac specific settings
;;----------------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-function-modifier 'hyper)

  (setq delete-by-moving-to-trash t)

  (setq-default locate-command "mdfind")

  ;; set font
  (set-frame-font "-*-Source Code Pro-medium-normal-medium-*-15-*-*-*-m-0-iso10646-1")
  (add-to-list 'default-frame-alist '(font . "-*-Source Code Pro-medium-normal-medium-*-15-*-*-*-m-0-iso10646-1"))

  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

;;----------------------------------------------------------------------------
;; End of Mac specific settings
;;----------------------------------------------------------------------------


;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst swanand-savefile-dir (expand-file-name "savefile" user-emacs-directory))

                                        ;create the savefile dir if it doesn't exist
(unless (file-exists-p swanand-savefile-dir)
  (make-directory swanand-savefile-dir))

(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-time-mode 1)

(add-hook 'prog-mode-hook 'linum-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 2)          ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Do not open GNU Emacs window on startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; maximize after opening
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

(setq read-file-name-completion-ignore-case t) ;; ignore case while searching for file
(put 'upcase-region 'disabled nil) ;; C-x C-u to upper case enabled
(put 'downcase-region 'disabled nil) ;; C-x C-l to upper case enabled

;; Trailing whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable transient mark mode
(transient-mark-mode 1)

(global-prettify-symbols-mode t)

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(save-place-mode 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; When you double-click on a file in the Mac Finder open it as a
;; buffer in the existing Emacs frame, rather than creating a new
;; frame just for that file.
(setq ns-pop-up-frames nil)

;; treat thisWord as 2 different words
(add-hook 'prog-mode-hook 'subword-mode)

(with-eval-after-load 'subword
  (diminish 'subword-mode))

(diminish 'auto-revert-mode)

;; Kill whole line
(setq kill-whole-line t)

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)


;;------------------
;; Custom Variables
;;------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c924950f6b5b92a064c5ad7063bb34fd3facead47cd0d761a31e7e76252996f7" default)))
 '(evil-escape-key-sequence "jk")
 '(evil-escape-unordered-key-sequence t)
 '(evil-leader/leader ",")
 '(package-selected-packages
   (quote
    (haskell-mode beacon neotree rvm chruby protobuf-mode evil-nerd-commenter web-mode emmet-mode hl-todo highlight-numbers rainbow-identifiers ace-window ace-jump-mode org-mode org-bullets company-quickhelp idle-highlight-mode dumb-jump swiper ox-jira ox-gfm scss-mode bundler rubocop rspec-mode ruby-tools ruby-hash-syntax smart-forward projectile-rails robe company-anaconda company-restclient pip-requirements anaconda-mode restclient git-timemachine company smartscan evil-escape evil-paredit evil-leader evil-matchit try helm-projectile yasnippet helm-swoop dired+ immortal-scratch js2-mode highlight-escape-sequences page-break-lines helm-spotify helm-gitignore docker docker-compose-mode flyspell-correct-helm csv-mode persistent-scratch move-text super-save paredit dockerfile-mode cask-mode yaml-mode markdown-mode dired flycheck flyspell-lazy diff-hl which-key undo-tree rainbow-mode rainbow-delimiters expand-region multiple-cursors nyan-mode magit-blame magit all-the-icons show-paren-mode powerline smart-mode-line monokai-theme use-package exec-path-from-shell auto-compile))))


;;------------------
;; Custom Functions
;;------------------


(defun view-buffer-name ()
  "Display the filename of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun scroll-down-five ()
  "Scrolls down five rows."
  (interactive)
  (scroll-down 5))


(defun scroll-up-five ()
  "Scrolls up five rows."
  (interactive)
  (scroll-up 5))


(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))


(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))


(defun copy-and-comment-region (beg end &optional arg)
  "Duplicate the region and comment-out the copied text."
  (interactive "r\nP")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region beg end arg)
  (goto-char end)
  (forward-line)
  (back-to-indentation-or-beginning-of-line))


(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))


(defun rename-buffer-and-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name))
        (read-file-name-function 'read-file-name-default))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))


(defun delete-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


(defun back-to-indentation-or-beginning-of-line ()
  "Move point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))


(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))


;; NOTE: (region-beginning) and (region-end) are not saved in
;; variables since they can change after each clean step.
(defun clean-up-buffer-or-region ()
  "Untabify, indent and delete trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (unless (or (eq major-mode 'coffee-mode)
                (eq major-mode 'feature-mode))
      (untabify (region-beginning) (region-end))
      (indent-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))


(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))


(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(defun sanityinc/flash-mode-line ()
  (interactive)
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq-default ring-bell-function 'sanityinc/flash-mode-line)

;;--------------------------------------------
;; Package configurations
;;--------------------------------------------
(use-package helm-swoop
  :init (global-unset-key (kbd "s-s"))
  :bind (("s-s s" . helm-swoop)
         ("s-s a" . helm-multi-swoop-all)
         ("s-s m" . helm-multi-swoop-current-mode)
         ("s-s p" . helm-multi-swoop-projectile)))

(use-package helm-projectile
  :bind ("M-p" . helm-projectile))

;; Referred from https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org#helm---interactive-completion
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          helm-locate-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-apropos-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-imenu-fuzzy-match t)

    (helm-mode))
  :bind (("s-p" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-mini)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c SPC" . helm-all-mark-rings)))

(use-package helm-ag
  :bind ("<f12>" . helm-projectile-ag))

(use-package evil-leader
  :init (evil-leader/set-key
          "f" 'helm-find-files
          "b" 'switch-to-buffer
          "p" 'helm-projectile)
  :config (global-evil-leader-mode))

(use-package evil
  :init
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  :bind (:map evil-insert-state-map
              ("C-k" . kill-line)
              ("C-y" . yank)
              ("C-a" . back-to-indentation-or-beginning-of-line)
              ("C-w" . kill-region-or-backward-word)
              ("C-e" . move-end-of-line))
  :config (evil-mode +1))

(use-package evil-surround
  :config (global-evil-surround-mode +1))

(use-package evil-matchit
  :config (global-evil-matchit-mode +1))

;; (use-package evil-nerd-commenter
;;   :bind ("M-;" . evilnc-comment-or-uncomment-Highlight))

(use-package evil-paredit
  :config (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

(use-package evil-escape
  :diminish evil-escape-mode
  :config (evil-escape-mode +1))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package beacon
  :diminish beacon-mode
  :config
  (setq beacon-push-mark 35
        beacon-color "Orange"
        beacon-blink-when-buffer-changes t
        beacon-blink-when-focused t
        beacon-blink-when-window-changes t
        beacon-blink-when-window-scrolls t)
  (beacon-mode 1))

(use-package smart-mode-line
  :config (progn
            (setq sml/theme 'automatic)
            (sml/setup))
  :init
  (progn
    (setq-default
     mode-line-format
     '("%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       mode-line-position
       (vc-mode vc-mode)
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces))))

;; show matching pairs
(show-paren-mode t)
(setq show-paren-delay 0.0)

(use-package all-the-icons)

(use-package neotree
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind ("<f8>" . neotree-project-dir)
  :config (evil-insert-state)) ;; because evil-normal state key bindings conflicts with neotree
;; If you want to configure normal state keybindings, you can refer below link:
;; https://www.emacswiki.org/emacs/NeoTree

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-c C-a" . magit-just-amend))
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
  (setq magit-set-upstream-on-push t)
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package git-timemachine)

(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)

         ("s-<" . mc/mark-previous-symbol-like-this)
         ("s->" . mc/mark-next-symbol-like-this)
         ("s-'" . mc/mark-all-symbols-like-this)

         ("C-c s-'" . mc/mark-all-symbols-like-this-in-defun)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package rainbow-delimiters)

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 120) ;; show chars after column 120 in different face
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))


(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))


(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-dictionary "en_GB-ize")
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-list-command "--list")
    (setq ispell-extra-args '("--sug-mode=ultra"
                              "--lang=en_GB"
                              "--run-together"
                              "--run-together-limit=5"
                              "--run-together-min=2"))))

(use-package flyspell-correct-helm
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-highlighting-mode 'lines)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Save history of minibuffer
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "minbuffer-history" user-emacs-directory))
  (savehist-mode +1))


(use-package dired
  :ensure nil
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq-default dired-listing-switches "-lhvA")

  ;; Kill buffers of files/directories that are deleted in dired.
  (setq dired-clean-up-buffers-too t)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :config (add-hook 'prog-mode-hook #'rainbow-mode))


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode)

(use-package cask-mode)

(use-package try)

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker)

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package rainbow-identifiers
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package super-save
  :demand
  :diminish super-save-mode
  :config
  (super-save-mode +1))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package ace-jump-mode
  :bind ("s-c" . ace-jump-mode))

(use-package ace-window
  :bind ("s-w" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dumb-jump
  :bind ("s-." . dumb-jump-go)
  :config (setq dumb-jump-selector 'helm))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package csv-mode
  :config
  (setq csv-separators '("," ";" "|" " "))
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'"))

(use-package helm-gitignore)

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(use-package highlight-escape-sequences
  :config
  (hes-mode t)
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face))

(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package js2-mode
  :defer t
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind ("C-a" . back-to-indentation-or-beginning-of-line)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))

(use-package json-mode)

;; (use-package pretty-mode
;;   :pin manual
;;   :config (add-hook 'prog-mode-hook 'pretty-mode))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" swanand-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; Undo and redo your window configurations
;; ~winner-mode~ lets you use ~C-c <left>~ and ~C-c <right>~ to switch between window configurations.
;; This is handy when something has popped up a buffer that you want to look at briefly before returning to whatever you were working on.
;; When you’re done, press ~C-c <left>~
(use-package winner
  :defer t
  :config (winner-mode +1))

(use-package yasnippet
  :defer t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)
    (setq yas-verbosity 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))


(use-package company
  :diminish company-mode
  :config
  ;; Offer idle completion for three characters or more. (1 is very
  ;; noisy, and 2 hurts typing performance a little.)
  (setq company-minimum-prefix-length 3)

  ;; Show a list of numbers next to completion options, where M-1
  ;; selects the first option and so on.
  (setq company-show-numbers t)

  ;; In the completion list, wrap around so going backwards from the
  ;; last option shows the first.
  (setq company-selection-wrap-around t)

  ;; Allow typing keys that don't match any candidates. This is useful
  ;; for imports, e.g. when we want to type foo::* in Rust but '*' isn't
  ;; in the candidates.
  (setq company-require-match nil)

  ;; Align annotations to they're not shown immediately next to the
  ;; candidate. Otherwise, we end with a function foo shown as "foof".
  (setq company-tooltip-align-annotations t)

  (global-company-mode))

(use-package company-quickhelp
  :diminish company-quickhelp-mode
  :config (company-quickhelp-mode 1))

;; https://github.com/pashky/restclient.el
(use-package restclient)

(use-package company-restclient
  :init (add-to-list 'company-backends 'company-restclient))

(use-package smart-forward
  :bind (("M-<up>" . smart-up)
         ("M-<down>" . smart-down)
         ("M-<left>" . smart-backward)
         ("M-<right>" . smart-forward)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package idle-highlight-mode
  :config (add-hook 'find-file-hook 'idle-highlight-mode))

;; Python
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :diminish anaconda-mode
  :diminish eldoc-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package pip-requirements
  :config (add-hook 'pip-requirements-mode-hook 'pip-requirements-auto-complete-setup))

;; TODO: execute ruby block and show it in comment like magic
;; (setq xmpfilter-command-name
;;       "ruby -S xmpfilter --no-warnings --dev --fork --detect-rbtest")
;; (require 'rcodetools)


;; http://haskell.github.io/haskell-mode/
(use-package haskell-mode)

;; Ruby and Rails
(use-package rvm
  :config (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))

(use-package ruby-hash-syntax)

(use-package ruby-tools
  :diminish ruby-tools-mode
  :config (add-hook 'ruby-mode-hook 'ruby-tools-mode))

(use-package robe
  :init (push 'company-robe company-backends))

(use-package projectile-rails
  :diminish projectile-rails-mode
  :init (setq projectile-rails-keymap-prefix (kbd "<f2>"))
  :bind (:map projectile-rails-mode-map ("M-<f1>" . robe-jump)))

(use-package ruby-mode
  :config (add-hook 'ruby-mode-hook 'projectile-rails-mode))

(use-package rspec-mode
  :no-require t
  :init (setq rspec-command-options "--color --order random")
  :config
  (rspec-install-snippets)
  (add-hook 'ruby-mode-hook 'rspec-mode))

(use-package rubocop)

(use-package bundler
  :no-require t)

;; Web files
(use-package web-mode
  :diminish web-mode
  :mode ("\\.html?\\'" "\\.erb\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-indent-style 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (rspec-mode +1)
  (projectile-rails-mode +1))

(use-package emmet-mode
  :diminish emmet-mode
  :config (add-hook 'web-mode-hook 'emmet-mode))

(use-package scss-mode
  :config (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

;; Org mode
(use-package ox-gfm)

(use-package org-bullets)

(use-package org
  :defer t
  :no-require t
  :bind ("C-c l" . org-store-link)
  :config
  (setq org-ellipsis "⤵")
  (setq org-src-fontify-natively t)
  (setq org-html-validation-link nil)
  (setq org-export-with-smart-quotes t)
  (setq org-src-window-setup 'current-window)
  ;; exporting to pdf
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (require 'ox-gfm nil t)

  ;; scratch buffer to load in org-mode
  (setq initial-major-mode 'org-mode))


;; Protocol buffers
(use-package protobuf-mode)

;; show comments in italics
(set-face-italic 'font-lock-comment-face t)
(set-face-italic 'font-lock-comment-delimiter-face t)

;; extra diminish
(with-eval-after-load 'hi-lock
  (diminish 'hi-lock-mode))

(diminish 'abbrev-mode)

;; PENDING ITEMS
;; code folding
;; multiple cursors: mark next && previous symbol not working
;; smartparens OR paredit?
;; all helm functionalities to be explored
;; projectile functionalities to be explored
;; org: I am just doing baby steps. Have to use it to its full capacity!
;; multiterm:
;; dired with colors
;; easy-kill

;;------------------
;; Key Bindings
;;------------------
(bind-key "RET" 'newline-and-indent)
(bind-key "C-c n" 'clean-up-buffer-or-region)

(bind-key "M-+" 'text-scale-increase)
(bind-key "M--" 'text-scale-decrease)

(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-below)
(bind-key "M-3" 'split-window-right)
(bind-key "M-0" 'delete-window)
(bind-key "M-`" 'other-frame)

(bind-key "M-g" 'goto-line)
(bind-key "C-a" 'back-to-indentation-or-beginning-of-line)

(bind-key "M-;" 'comment-or-uncomment-current-line-or-region)

(bind-key "M-]" 'next-buffer)
(bind-key "M-[" 'previous-buffer)

(bind-key "M-s" 'save-buffer)

(bind-key "M-*" 'pop-tag-mark)

(bind-key "C-c s" 'swap-windows)
(bind-key "C-c r" 'rename-buffer-and-file)
(bind-key "C-c k" 'delete-buffer-and-file)

(bind-key "s-[" 'scroll-down-five)
(bind-key "s-]" 'scroll-up-five)

(bind-key "M-/" 'hippie-expand)

(bind-key "C-c c" 'copy-and-comment-region)

;; join lines
(bind-key "M-j" (lambda () (interactive) (join-line -1)))

(bind-key "<C-return>" 'open-line-below)
(bind-key "<C-S-return>" 'open-line-above)

;; Ask for confirmation
(bind-key "C-x C-c" (lambda () (interactive)
                      (if (y-or-n-p "Quit Emacs? ")
                          (save-buffers-kill-emacs))))
