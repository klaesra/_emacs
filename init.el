;; My own additions!

;; Add my load paths
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode")
(add-to-list 'load-path "/usr/bin/")
(add-to-list 'load-path "/home/klaes/lib/.emacs.d/site-lisp/")

;; Always use linum-mode for line numbers
(global-linum-mode 1)
;; But dont use it when it makes emacs freeze!
(require 'linum-off)

;; Ignore warning about .emacs.d in load path
;; It resides in the .emacs.d/init.el file
;; (defun string-starts-with (string prefix)
;;   "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
;;   (and (>= (length string) (length prefix))
;;        (string-equal (substring string 0 (length prefix)) prefix)))

;; (defadvice display-warning
;;     (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
;;   "Ignore the warning about the `.emacs.d' directory being in `load-path'."
;;   (unless (and (eq type 'initialization)
;;                (string-starts-with message "Your `load-path' seems to contain\nyour `.emacs.d' directory"))
;;     ad-do-it))

;; Haskell config
(autoload 'ghc-init "ghc-core" nil t)
(autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)
(autoload 'turn-on-haskell-indent "haskell-indent" "Indentation mode for Haskell" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(require 'ghc-core)

;; LaTeX config

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") 'ace-jump-mode))

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-:") 'flyspell-auto-correct-previous-word))

;; Multiple cursors keybinds
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-;") 'ace-jump-mode)

;; PHP-mode
(require 'php-mode)

;; Reload emacs file
(defun reload () (interactive)
  "Reload ~/.emacs"
  (if (file-exists-p "/home/klaes/.emacs.d/init.el")
      (load-file "/home/klaes/.emacs.d/init.el")))

;; Make default browser conkeror
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/conkeror")

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Set font size
(set-face-attribute 'default nil :height 145)

;; Remove duplicate lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

;; Make tramp use zsh shell
(eval-after-load 'tramp '(setenv "SHELL" "/bin/zsh"))

;; Advise to open file using sudo if neccessary
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Prevent magit from tossing a warning each time i start!
(setq magit-last-seen-setup-instructions "1.4.0")
