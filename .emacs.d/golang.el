;;; package --- Summary
;;; Commentary:

;;; Dependencies:
;; go get -u github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/goimports
;; go get github.com/rogpeppe/godef



;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct

;;; TODO:
;; gorename

;; ------------ hmmm...
;; go get golang.org/x/tools/cmd/gorename
;; go build golang.org/x/tools/cmd/gorename
;; mv gorename $HOME/bin/

(setq-default gofmt-command "goimports")

(defun aggressive-go-mode ()
  "Documented string."
  (aggressive-indent-mode t)
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )


(add-hook 'go-mode-hook 'aggressive-go-mode)

;;; Code:
(use-package go-mode)

;; go-code refrence
(use-package go-guru
  :config
  (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx bos "*go-guru-output*" eos)
  ;;                (display-buffer-reuse-window
  ;;                 display-buffer-in-side-window)
  ;;                (side            . bottom)
  ;;                (reusable-frames . visible)
  ;;                (window-height   . 0.33)))
  )

(use-package go-fill-struct)

;; quick docs in minibuffer
(use-package go-eldoc
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

;; linter
;; golangci-lint can't run linters on fly, because it needs a file-tree to analyze
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-enable-all t)
  (setq flycheck-golangci-lint-disable-linters '("lll" "dupl" "maligned" "prealloc" "gochecknoglobals" "gosec"))
  (setq flycheck-golangci-lint-fast t))

;; autocomplete
(use-package company-go
  :config
  (add-to-list 'company-backends 'company-go)
  ;; (add-hook 'go-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'company-backends) '(company-go))
  ;;             ;; (company-mode)
  ;;             ))
  )


;;; golang.el ends here
