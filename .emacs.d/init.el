;;; package -- Summary
;;; Commentary:
;;
;;    (      )
;;    ))    ((
;;   //      \\
;;  | \\____// |
;; \~/ ~    ~\/~~/
;;  (|    _/o  ~~
;;   /  /     ,|
;;  (~~~)__.-\ |
;;   ``~~    | |
;;    |      | |
;;    |        |
;;   /          \
;;  `\          /'
;;    `\_    _/'
;;       ~~~~
;;
;;
;; go get github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/guru
;; go get -u github.com/jstemmer/gotags
;; go get -u github.com/dougm/goflymake
;; ~~~~~~~~~~~~
;;; Code:

(require 'package)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-quickstart t)


;; ~~~ CORE ~~~

(defun mega-prog ()
  "My 'prog-mode' hook."
  (setq tab-width 2)
  (aggressive-indent-mode t)
  (indent-guide-mode))
(add-hook 'prog-mode-hook 'mega-prog)

(defun my-go-mode-hook ()
  "My go-mode hook."
  (setq gofmt-command "goimports")
  (go-eldoc-setup))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Start the Emacs server so other clients can connect and use the same session.
;; This is useful for when you may be oprating Emacs from the GUI usually,
;; but want to use the same session from a TTY/terminal.
;; Also handy for when you have your EDITOR set to emacsclient.
(server-start)

;; Configure Emacs for full UTF-8 compatability
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; automaticly revert doc-view-buffers when the file changes on disk.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill buffer after terminal exit."
  (kill-buffer))

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq-default cursor-in-non-selected-windows t)
(setq x-underline-at-descent-line t)
(electric-pair-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(add-hook 'prog-mode-hook #'linum-mode) ;; Line numbers

(setq-default auto-window-vscroll nil
              fill-column 80
              help-window-select t
              sentence-end-double-space nil
              show-trailing-whitespace nil)

;; Smooth scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-margin 1)
(setq scroll-step 1)
(save-place-mode 1) ;; save cursor position
(setq inhibit-startup-screen t) ;; disable startup screen
(setq confirm-kill-emacs 'yes-or-no-p)
(setq initial-scratch-message (format ";; %s" (current-time-string)))

(setq
 make-backup-files nil
 auto-save-default nil
 ring-bell-function 'ignore)

;; set tramp shell prompt pattern (fix for some fancy prompts)
(setq-default shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; use ‘root’ user by default for ssh connections using tramp
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; ~~~ PACKAGES ~~~

;; set use-package :ensure to always true (:ensure nil - false)
(setq use-package-always-ensure t)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  (setq evil-normal-state-cursor '(box "gainsboro")
        evil-insert-state-cursor '((bar . 2) "light steel blue")
        evil-visual-state-cursor '((hbar . 2) "light steel blue")))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package general
  :config (general-define-key
           :states '(normal visual)
           :prefix ","
           "1" '(eyebrowse-switch-to-window-config-1 :which-key "layout 1")
           "2" '(eyebrowse-switch-to-window-config-2 :which-key "layout 2")
           "3" '(eyebrowse-switch-to-window-config-3 :which-key "layout 3")
           "4" '(eyebrowse-switch-to-window-config-4 :which-key "layout 4")
           "5" '(eyebrowse-switch-to-window-config-5 :which-key "layout 5")
           "j" '(avy-goto-char-timer :which-key "avy-jump")
           "/" '(swiper :which-key "swiper")
           "*" '(counsel-projectile-grep :which-key "grep-regex")
           "t" '(ansi-term :which-key "terminal")
           "p" '(counsel-yank-pop :which-key "yank-pop")
           "TAB" '(alternate-buffer :which-key "alternate-buffer")
           "i" '(counsel-imenu :which-key "imenu")

           "f" '(:ignore t :which-key "files")
           "ff" '(counsel-find-file :which-key "find")
           "fp" '(counsel-projectile-find-file :which-key "projectile-find-file")
           "fr" '(counsel-recentf :which-key "recent")
           "fo" '(find-file-other-frame :which-key "dired-other-frame")

           "b" '(:ignore t :which-key "buffers")
           "bb" '(counsel-ibuffer :which-key "ivy-switch")
           "bi" '(ibuffer :which-key "ibuffer")
           "br" '(rename-buffer :which-key "rename")

           "g"  '(:ignore t :which-key "golang")
           "gf" '(gofmt :which-key "gofmt")
           "ga" '(go-import-add :which-key "add-import")
           "gr" '(go-remove-unused-imports :which-key "remove-unused-imports")
           "gj" '(godef-jump :which-key "godef-jump")
           "gJ" '(go-guru-referrers :which-key "guru-referrers")
           "gd" '(godef-describe :which-key "godef-describe")
           "gc" '(godoc-at-point :which-key "godoc-at-point")

           "c"  '(:ignore t :which-key "flycheck")
           "cj" '(flycheck-next-error :which-key "next-error")
           "ck" '(flycheck-previous-error :which-key "previous-error")
           "cl" '(flycheck-list-errors :which-key "list-errors")

           "l" '(:ignore t :which-key "lisp")
           "le" '(eval-region :which-key "eval-region")
           ))

;; Autocomplete
(use-package company
  :diminish company-mode
  :config (add-hook 'prog-mode-hook 'company-mode)
  (setq company-tooltip-minimum-width 30)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous)
  (define-key prog-mode-map (kbd "C-SPC") 'company-complete)
  (define-key prog-mode-map (kbd "C-SPC") 'company-complete))

;; ~~~ GO ~~~

(use-package go-mode)

;; go-code refrence
(use-package go-guru
  :config
  (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*go-guru-output*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(use-package company-go
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  (custom-set-faces '(company-preview ((t (:underline nil))))))


(use-package go-autocomplete)
(use-package go-complete)
(use-package go-dlv)
(use-package go-errcheck)
(use-package go-gopath)
(use-package go-imenu)
(use-package go-impl)
(use-package go-imports)

;; ;; Linting
;; (use-package flycheck-gometalinter
;;   :init
;;   ;; skip linting for vendor dirs
;;   (setq flycheck-gometalinter-vendor t)
;;   ;; use in test files
;;   (setq flycheck-gometalinter-test t)
;;   ;; only use fast linters
;;   (setq flycheck-gometalinter-fast t)
;;   ;; explicitly disable 'gotype' & 'govet' linters (also currently broken Nix overlays)
;;   (setq flycheck-gometalinter-disable-linters '("gotype" "vet" "vetshadow" "megacheck" "interfacer" "ineffassign"))
;;   :config (progn (flycheck-gometalinter-setup)))

;; layouts
(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  (setq-default eyebrowse-new-workspace t))

;; indentation levels
(use-package indent-guide
  :diminish indent-guide-mode)

;; restclient - orgstruct for .http files
(use-package restclient
  :config
  (defun http-restclient ()
    "Use restclient mode in .http files."
    (when (and (stringp buffer-file-name)
               (string-match "\\.http\\'" buffer-file-name))
      (restclient-mode)
      (orgstruct-mode)
      (linum-mode)
      (setq-default orgstruct-heading-prefix-regexp "\\#+\\")))
  (add-hook 'find-file-hook 'http-restclient))

;; (use-package shackle
;;   :defer 1
;;   :config
;;   (setq-default
;;    shackle-rules '((help-mode :inhibit-window-quit t :same t))
;;    shackle-select-reused-windows t)
;;   (shackle-mode 1))

;; Syntax check
(use-package flycheck
  :diminish flycheck-mode
  :init (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)


  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

;; Ivy
(use-package counsel
  :diminish ivy-mode
  :init
  (setq counsel-yank-pop-separator
        (concat "\n\n" (concat (apply 'concat (make-list 50 "---")) "\n")))
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-ignore-order)
          (t . ivy--regex-fuzzy))))

(use-package neotree
  :config (global-set-key (kbd "M-1") 'neotree-toggle)
  (setq neo-theme 'arrow)
  (setq neo-window-width 35)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq-default neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-force-change-root t)

  (evil-define-key 'normal neotree-mode-map
    (kbd "RET") (neotree-make-executor
                 :file-fn 'neo-open-file
                 :dir-fn 'neo-open-dir)
    (kbd "TAB") (neotree-make-executor
                 :dir-fn 'neo-open-dir)
    "z" (neotree-make-executor
         :dir-fn 'neo-open-dir)
    "ZZ" 'quit-window
    "gd" (neotree-make-executor
          :dir-fn 'neo-open-dired)
    "gD" (neotree-make-executor
          :dir-fn 'neo-open-dired)
    "go" (neotree-make-executor
          :file-fn 'neo-open-file
          :dir-fn 'neo-open-dir)
    "gO" 'neotree-quick-look
    "gr" 'neotree-refresh
    "q" 'neotree-hide
    "H" 'neotree-hidden-file-toggle
    "gh" 'neotree-hidden-file-toggle
    (kbd "C-k") 'neotree-select-up-node
    "gk" 'neotree-select-up-node
    "[" 'neotree-select-up-node
    (kbd "C-j") 'neotree-select-down-node
    "gj" 'neotree-select-down-node
    "]" 'neotree-select-down-node
    "gv" 'neotree-open-file-in-system-application
    "c" 'neotree-create-node
    "y" 'neotree-copy-node
    "r" 'neotree-rename-node
    "R" 'neotree-change-root
    "d" 'neotree-delete-node
    "J" 'neotree-dir
    "+" 'neotree-stretch-toggle
    "=" 'neotree-stretch-toggle
    "ge" 'neotree-enter
    "j" 'neotree-next-line
    "k" 'neotree-previous-line

    ;; Unchanged keybings.
    "a" (neotree-make-executor
         :file-fn 'neo-open-file-ace-window)
    "|" (neotree-make-executor
         :file-fn 'neo-open-file-vertical-split)
    "-" (neotree-make-executor
         :file-fn 'neo-open-file-horizontal-split)
    "S" 'neotree-select-previous-sibling-node
    "s" 'neotree-select-next-sibling-node
    (kbd "C-c C-c") 'neotree-change-root
    (kbd "C-x 1") 'neotree-empty-fn
    (kbd "C-x 2") 'neotree-empty-fn
    (kbd "C-x 3") 'neotree-empty-fn
    (kbd "C-x C-f") 'find-file-other-window
    (kbd "C-c C-f") 'find-file-other-window)
  )

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :config (projectile-mode t))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package diminish
  :config
  (progn
    (diminish 'undo-tree-mode)
    (diminish 'eldoc-mode)))

(use-package evil-escape
  :config
  (global-set-key (kbd "<escape>") 'evil-escape))

;; (use-package magit
;;   :init
;;   (setq magit-completing-read-function 'ivy-completing-read))

;; (use-package evil-magit)

;; (use-package diff-mode
;;   :ensure nil
;;   :config
;;   (set-face-attribute 'diff-added nil :background nil)
;;   (set-face-attribute 'diff-removed nil :background nil))
;; (use-package ediff-init
;;   :ensure nil
;;   :config
;;   (me/unboldify '(ediff-fine-diff-A
;;                   ediff-fine-diff-B
;;                   ediff-fine-diff-C)))

;; (use-package ediff-wind
;;   :ensure nil
;;   :config
;;   (setq-default
;;    ediff-split-window-function #'split-window-horizontally
;;    ediff-window-setup-function #'ediff-setup-windows-plain))

;; (use-package smerge-mode
;;   :ensure nil
;;   :config
;;   (zenburn-with-color-variables
;;    (set-face-attribute 'smerge-mine nil :background zenburn-red-2)
;;    (set-face-attribute 'smerge-other nil :background zenburn-green)
;;    (set-face-attribute 'smerge-refined-added nil :background zenburn-green-1)
;;    (set-face-attribute 'smerge-refined-removed nil :background zenburn-red-4)))

(use-package dired
  :ensure nil
  :delight dired-mode "Dired"
  :preface
  (defun me/dired-directories-first ()
    "Sort dired listings with directories first before adding marks."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (advice-add 'dired-readin :after #'me/dired-directories-first)
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda () (interactive) (find-alternate-file "..")))))
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (setq-default
   dired-auto-revert-buffer t
   dired-dwim-target t
   dired-hide-details-hide-symlink-targets nil
   dired-listing-switches "-alh"
   dired-ls-F-marks-symlinks nil
   dired-recursive-copies 'always))

;; Improve fuzzy
(use-package flx)

;; sexy color scheme
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t))

;; Easy-motion
(use-package avy)

;; "gc" for comments
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode 1))

;; which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :diminish which-key-mode
  :config (which-key-mode))

;; highligh current line-number
(use-package hlinum
  :config (hlinum-activate))

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

(use-package highlight-parentheses
  :diminish 'highlight-parentheses-mode
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (setq hl-paren-colors '("Springgreen3" "IndianRed1" "IndianRed3" "IndianRed4" "firebrick4" "red4" "red4" "red4" "red4")))

;; just highlight numbers in buffer
(use-package highlight-numbers
  :defer t
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (setq-default aggressive-indent-comments-too t))

(use-package json-mode)
(use-package yaml-mode)
(use-package js2-mode :mode "\\.js\\'")

;; ~~~ CUSTOM ~~~

(defun set-initial-frame-size (h-margin v-margin centering)
  "Set Emacs startup windows size relative to monitor parameters V-MARGIN, H-MARGIN are parameters in percents."
  (when (display-graphic-p)
    (setq monitor-width (nth 3 (nth 2 (frame-monitor-attributes))))
    (setq monitor-height (nth 4 (nth 2 (frame-monitor-attributes))))
    (setq h-px (/ (* monitor-width h-margin) 100))
    (setq v-px (/ (* monitor-height v-margin) 100))
    (add-to-list 'initial-frame-alist `(width . ,(/ h-px (frame-char-width))))
    (add-to-list 'initial-frame-alist `(height . ,(/ v-px (frame-char-height))))
    (when centering
      (progn
        (add-to-list 'initial-frame-alist `(left . ,(/ (- monitor-width h-px) 2)))
        (add-to-list 'initial-frame-alist `(top . ,(/ (- monitor-height v-px) 2)))))))
(set-initial-frame-size 80 80 nil)

(defun linum-format-func (line)
  "Add space at the right side of number LINE."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format " %%%dd" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun alternate-buffer
    (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer (cl-find-if (lambda (buffer)
                                    (not (eq buffer current-buffer)))
                                  (mapcar #'car (window-prev-buffers window))))))

(defun term-mode-paste ()
  "Paste into Emacs terminal like with bash."
  (define-key term-raw-map (kbd "C-S-v") 'term-paste))
(add-hook 'term-mode-hook 'term-mode-paste)

;; ~~~ END ~~~

;; (put 'narrow-to-page 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(persp-mode go-direx indent-guide centered-window yaml-mode which-key use-package restclient popwin perspective neotree kaolin-themes json-mode hlinum highlight-parentheses highlight-numbers go-guru go-fill-struct go-eldoc general focus flycheck-gometalinter evil-surround evil-smartparens evil-quickscope evil-magit evil-lion evil-goggles evil-exchange evil-escape evil-commentary diminish counsel-projectile company-posframe company-go company-flx avy-flycheck auto-complete aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:underline nil))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
