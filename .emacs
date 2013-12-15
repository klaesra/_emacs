;;
;; File .emacs - These commands are executed when GNU emacs starts up.
;;
;; $Id: .emacs,v 1.8 1995/11/07 20:12:07 dewell Exp $
;; revised 8/15/2009
;;
;; Now, it resides as .emacs.d/init.el

;; Add extra load path to emacs
(add-to-list 'load-path "/home/klaes/lib/_emacs/")
(add-to-list 'load-path "/home/klaes/lib/web-mode/")
(add-to-list 'load-path "/home/klaes/.cabal/share/ghc-mod-1.11.0/")
(add-to-list 'load-path "/home/klaes/lib/emacs-flymake/")
(add-to-list 'load-path "/usr/bin/")
(setenv "PATH"
        (concat (expand-file-name "/home/klaes/bin/")
                path-separator (getenv "PATH")))
(setq exec-path (append exec-path '("/home/klaes/bin")))

;; Using MELPA
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Multiple cursors in emacs
(add-to-list 'load-path "/usr/share/emacs/site-lisp/multiple-cursors")
(require 'multiple-cursors)
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Enable magit for git in emacs
(require 'magit)

;; Auto completion mode in emacs
(add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)

;; Use zsh mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; Auto indent mode in emacs
(require 'auto-indent-mode)

;; Extra modes from magnars
(add-to-list 'load-path "/home/klaes/lib/magnars/.emacs.d/")
(add-to-list 'load-path "/home/klaes/lib/magnars/.emacs.d/site-lisp/")
(add-to-list 'load-path "/home/klaes/lib/magnars/.emacs.d/site-lisp/expand-region/")
;;(add-to-list 'load-path "/home/klaes/lib/magnars/.emacs.d/site-lisp/rhtml-mode/")

(load "site-lisp/s/s.el")

(load "site-lisp/expand-region/expand-region.el")
(global-set-key (kbd "C-=") 'er/expand-region)

(load "defuns/lisp-defuns.el")
(global-set-key (kbd "C-x C-e") 'eval-and-replace)

(load "defuns/buffer-defuns.el")
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(defadvice sgml-delete-tag (after reindent-buffer activate)
  (cleanup-buffer))

(load "ace-jump-mode/ace-jump-mode.el")
(global-set-key (kbd "C-'") 'ace-jump-mode)

;; Extra keybinds
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;;;; web-mode (php and html, etc mode, git maintained)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;(set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
                                        ;(define-key web-mode-map (kbd "C-n") 'web-mode-match-tag)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq auto-complete-mode t)
  (setq debug-on-error t)
  )
(add-hook 'web-mode-hook 'web-mode-hook)
(defadvice web-mode-element-close (after reindent-buffer activate)
  (cleanup-buffer))




;; Add ghc-mode hook
(require 'flymake) 
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; Keep Emacs from executing file local variables.
;; (this is also in the site-init.el file loaded at emacs dump time.)
(setq inhibit-local-variables t  ; v18
      enable-local-variables nil ; v19
      enable-local-eval nil)     ; v19)

;; Cause the region to be highlighted and prevent region-based commands
;; from running when the mark isn't active.

(pending-delete-mode t)
(setq transient-mark-mode t)


;; Fonts are automatically highlighted.  For more information
;; type M-x describe-mode font-lock-mode

(global-font-lock-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This provides customized support for writing programs in different kinds
;;;; of programming languages.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Haskell mode
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;________________________________
;;    ruby-mode
;;________________________________

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode" t)

(setq ruby-indent-level 4)
;; Treat .rjs files as Ruby
(setq auto-mode-alist (cons '("\\.rjs\\'" . ruby-mode) auto-mode-alist))
;; Rakefiles are Ruby files:
(setq auto-mode-alist (cons '("\\Rakefile\\'" . ruby-mode) auto-mode-alist))
;; So is Gemfile:
(setq auto-mode-alist (cons '("\\Gemfile\\'" . ruby-mode) auto-mode-alist))

;; Fix curly brackets for ruby mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "{" nil)
            (define-key ruby-mode-map "}" nil)))


;; Erlang mode
;;(setq erlang-root-dir "/usr/lib/erlang")
;;(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;;(require 'erlang-start)

;; Load the C++ and C editing modes and specify which file extensions
;; correspond to which modes.
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode "c-mode" "C Editing Mode"   t)
(setq auto-mode-alist
      (append '(("\\.C$"   . c++-mode)
                ("\\.cc$"  . c++-mode)
                ("\\.icc$" . c++-mode)
                ("\\.c$"   . c-mode)
                ("\\.y$"   . c-mode)
                ("\\.h$"   . c++-mode))
              auto-mode-alist))

;; set tab distance to something, so it doesn't change randomly and confuse people
(setq c-basic-offset 4)

;; This function is used in various programming language mode hooks below.  It
;; does indentation after every newline when writing a program.

(defun newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent))


;; Tell Emacs to use the function above in certain editing modes.

(add-hook 'lisp-mode-hook             (function newline-indents))
(add-hook 'emacs-lisp-mode-hook       (function newline-indents))
(add-hook 'lisp-interaction-mode-hook (function newline-indents))
(add-hook 'scheme-mode-hook           (function newline-indents))
(add-hook 'c-mode-hook                (function newline-indents))
(add-hook 'c++-mode-hook              (function newline-indents))
(add-hook 'java-mode-hook             (function newline-indents))


;; Fortran mode provides a special newline-and-indent function.

(add-hook 'fortran-mode-hook
          (function (lambda ()
                      (local-set-key "\C-m" 'fortran-indent-new-line))))


;; Text-based modes (including mail, TeX, and LaTeX modes) are auto-filled.

(add-hook 'text-mode-hook (function turn-on-auto-fill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This makes "M-x compile" smarter by trying to guess what the compilation
;;;; command should be for the C, C++, and Fortran language modes.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; By requiring `compile' at this point, we help to ensure that the global
;; value of compile-command is set properly.  If `compile' is autoloaded when
;; the current buffer has a buffer-local copy of compile-command, then the
;; global value doesn't get set properly.

(require 'compile)


;; This gives the form of the default compilation command for C++, C, and
;; Fortran programs.  Specifying the "-lm" option for C and C++  eliminates a
;; lot of potential confusion.

(defvar compile-guess-command-table
  '((c-mode       . "gcc -Wall -g %s -o %s -lm"); Doesn't work for ".h" files.
    (c++-mode     . "g++ -g %s -o %s -lm")      ; Doesn't work for ".h" files.
    (fortran-mode . "f77 -C %s -o %s")
    )
  "*Association list of major modes to compilation command descriptions, used
by the function `compile-guess-command'.  For each major mode, the compilation
command may be described by either:

  + A string, which is used as a format string.  The format string must accept
    two arguments: the simple (non-directory) name of the file to be compiled,
    and the name of the program to be produced.

  + A function.  In this case, the function is called with the two arguments
    described above and must return the compilation command.")


;; This code guesses the right compilation command when Emacs is asked
;; to compile the contents of a buffer.  It bases this guess upon the
;; filename extension of the file in the buffer.

(defun compile-guess-command ()

  (let ((command-for-mode (cdr (assq major-mode
                                     compile-guess-command-table))))
    (if (and command-for-mode
             (stringp buffer-file-name))
        (let* ((file-name (file-name-nondirectory buffer-file-name))
               (file-name-sans-suffix (if (and (string-match "\\.[^.]*\\'"
                                                             file-name)
                                               (> (match-beginning 0) 0))
                                          (substring file-name
                                                     0 (match-beginning 0))
                                        nil)))
          (if file-name-sans-suffix
              (progn
                (make-local-variable 'compile-command)
                (setq compile-command
                      (if (stringp command-for-mode)
                          ;; Optimize the common case.
                          (format command-for-mode
                                  file-name file-name-sans-suffix)
                        (funcall command-for-mode
                                 file-name file-name-sans-suffix)))
                compile-command)
            nil))
      nil)))


;; Add the appropriate mode hooks.

(add-hook 'c-mode-hook       (function compile-guess-command))
(add-hook 'c++-mode-hook     (function compile-guess-command))
(add-hook 'fortran-mode-hook (function compile-guess-command))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This creates and adds a "Compile" menu to the compiled language modes.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar compile-menu nil
  "The \"Compile\" menu keymap.")

(defvar check-option-modes nil
  "The list of major modes in which the \"Check\" option in the \"Compile\"
menu should be used.")

(defvar compile-menu-modes nil
  "The list of major modes in which the \"Compile\" menu has been installed.
This list used by the function `add-compile-menu-to-mode', which is called by
various major mode hooks.")


;; Create the "Compile" menu.

(if compile-menu
    nil
  (setq compile-menu (make-sparse-keymap "Compile"))
  ;; Define the menu from the bottom up.
  (define-key compile-menu [first-error] '("    First Compilation Error" .
                                           first-compilation-error))
  (define-key compile-menu [prev-error]  '("    Previous Compilation Error" .
                                           previous-compilation-error))
  (define-key compile-menu [next-error]  '("    Next Compilation Error" .
                                           next-error))
  (define-key compile-menu [goto-line]   '("    Line Number..." .
                                           goto-line))

  (define-key compile-menu [goto]        '("Goto:" . nil))
  ;;
  (define-key compile-menu [indent-region] '("Indent Selection" .
                                             indent-region))

  (define-key compile-menu [make]         '("Make..." . make))

  (define-key compile-menu [check-file]   '("Check This File..." .
                                            check-file))

  (define-key compile-menu [compile]     '("Compile This File..." . compile))
  )


;;; Enable check-file only in Fortran mode buffers

(put 'check-file 'menu-enable '(eq major-mode 'fortran-mode))


;;; Here are the new commands that are invoked by the "Compile" menu.

(defun previous-compilation-error ()
  "Visit previous compilation error message and corresponding source code.
See the documentation for the command `next-error' for more information."
  (interactive)
  (next-error -1))

(defun first-compilation-error ()
  "Visit the first compilation error message and corresponding source code.
See the documentation for the command `next-error' for more information."
  (interactive)
  (next-error '(4)))

(defvar check-history nil)

(defun check-file ()
  "Run ftnchek on the file contained in the current buffer"
  (interactive)
  (let* ((file-name (file-name-nondirectory buffer-file-name))
         (check-command (read-from-minibuffer
                         "Check command: "
                         (format "ftnchek %s" file-name) nil nil
                         '(check-history . 1))))
    (save-some-buffers nil nil)
    (compilation-start check-command "Can't find next/previous error"
                       "Checking" nil)))

(defun make ()
  "Run make in the directory of the file contained in the current buffer"
  (interactive)
  (save-some-buffers nil nil)
  (compilation-start (read-from-minibuffer "Make command: " "make ")
                     "Can't find next/previous error" "Make"
                     nil))


;;; Define a function to be called by the compiled language mode hooks.

(defun add-compile-menu-to-mode ()
  "If the current major mode doesn't already have access to the \"Compile\"
 menu, add it to the menu bar."
  (if (memq major-mode compile-menu-modes)
      nil
    (local-set-key [menu-bar compile] (cons "Compile" compile-menu))
    (setq compile-menu-modes (cons major-mode compile-menu-modes))
    ))


;; And finally, make sure that the "Compile" menu is available in C, C++, and
;; Fortran modes.
(add-hook 'c-mode-hook       (function add-compile-menu-to-mode))
(add-hook 'c++-c-mode-hook   (function add-compile-menu-to-mode))
(add-hook 'c++-mode-hook     (function add-compile-menu-to-mode))
(add-hook 'fortran-mode-hook (function add-compile-menu-to-mode))

;; To make emacs use spaces instead of tabs (Added by Art Lee on 2/19/2008)
(setq-default indent-tabs-mode nil)

;; This is how emacs tells the file type by the file suffix.
(setq auto-mode-alist
      (append '(("\\.mss$" . scribe-mode))
              '(("\\.bib$" . bibtex-mode))
              '(("\\.tex$" . latex-mode))
              '(("\\.obj$" . lisp-mode))
              '(("\\.st$"  . smalltalk-mode))
              '(("\\.Z$"   . uncompress-while-visiting))
              '(("\\.cs$"  . indented-text-mode))
              auto-mode-alist))
;;
;; Finally look for .customs.emacs file and load it if found

(if "/home/klaes/.customs.emacs"
    (load "/home/klaes/.customs.emacs" t t))

;; Art: added with v. 23.1 to make spacebar complete filenames (8/17/2009)
(progn
  (define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)
  (define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
  (define-key minibuffer-local-must-match-map " " 'minibuffer-complete-word))

;; Art: added with v. 23.1
;; Set env variable this way?  I used the traditional way instead
                                        ;(info "(emacs) Windows HOME")

;; End of file.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit (default family consolas) :stipple nil :background "black" :foreground "gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "fixed"))))
 '(erc-input-face ((t (:foreground "deep sky blue"))))
 '(erc-my-nick-face ((t (:foreground "cyan" :weight bold)))))

;; Make emacs load maximized under windows.
                                        ;(defun w32-maximize-frame ()
                                        ;  "Maximize the current frame"
                                        ;  (interactive)
                                        ;  (w32-send-sys-command 61488))

                                        ;(add-hook 'window-setup-hook 'w32-maximize-frame t)


;; My own settings:
                                        ;Compiler med pdflatex
(setq TeX-PDF-mode t)

                                        ;Set my email adress
(setq mail-host-address "klaes.dk")

                                        ;Fjern statup msg!!!
(setq inhibit-startup-message t)

                                        ;Fjern scratch message
(setq initial-scratch-message nil)

                                        ;Fjern toolbar og scroll
(tool-bar-mode 0)
(toggle-scroll-bar 0)

;; function to reload .emacs
(defun reload () (interactive)
  "Reload ~/.emacs"
  (if (file-exists-p "/home/klaes/.emacs")
      (load-file "/home/klaes/.emacs")))

                                        ;battery showgo
(display-battery-mode t)
(show-paren-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; To customize the background color
(set-face-background 'hl-line "#330")

;; Define the way Emacs looks:
(set-foreground-color "gray")
(set-background-color "black")
(set-cursor-color "red")
(set-mouse-color "green")
(set-border-color "light green")
(menu-bar-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set emacs to reload last used buffers
(desktop-save-mode 1)

;; Allow emacs to use some otherwise dead latin characters
(load-library "iso-transl")

(add-hook 'find-file-hook (lambda () (linum-mode 1)))

(global-linum-mode 1)

(setq linum-format "%d ")


;; Set default font for emacs
(set-frame-font "DejaVu Sans Mono-11")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings for irc things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

(load-file "/home/klaes/.private.el")

;; Render html with w3m
(require 'w3m-load)
(add-hook 'erc-insert-modify-hook 'mah/maybe-wash-im-with-w3m)
(autoload 'w3m-region "w3m" "Render region using w3m")
(defun mah/maybe-wash-im-with-w3m ()
  "Wash the current im with emacs-w3m."
  (save-restriction
    (with-current-buffer (current-buffer)
      (let ((case-fold-search t))
        (goto-char (point-min))
        (when (re-search-forward "<HTML>.*</HTML>" nil t)
          (print (match-string 0))
          (narrow-to-region (match-beginning 0) (match-end 0))
          (let ((w3m-safe-url-regexp mm-w3m-safe-url-regexp)
                w3m-force-redisplay)
            (w3m-region (point-min) (point-max))
            (goto-char (point-max))
            (delete-char -2))
          (when (and mm-inline-text-html-with-w3m-keymap
                     (boundp 'w3m-minor-mode-map)
                     w3m-minor-mode-map)
            (add-text-properties
             (point-min) (point-max)
             (list 'keymap w3m-minor-mode-map
                   ;; Put the mark meaning this part was rendered by emacs-w3m.
                   'mm-inline-text-html-with-w3m t))))))))

;; Join channels and servers
(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)

(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   bitlbee-password))))

(add-hook 'erc-join-hook 'freenode-identify)
(defun freenode-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "irc.freenode.net" erc-session-server)
             (string= "NickServ"))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   freenode-password))))



(setq erc-autojoin-channels-alist
      '(("freenode.net" "#diku" "#zomg_pwnies")))


(defun bitlbee () (interactive)
  (erc :server "localhost" :port 6667 :nick "klaes")
  )

(defun freenode () (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "klaes")
  )


(setq erc-auto-discard-away t)

(defvar my-erc-page-message "%s is talking to you!"
  "Format of message to display in dialog box")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
 notification")

(defvar my-erc-page-timeout 30
  "Number of seconds that must elapse between notifications from
 the same person.")

(defun my-emacs-is-idle (&optional secs)
  "Return a boolean whether emacs has been idle for more than
   'secs', defaults to 20."
  (< (or secs my-erc-page-timeout)
     (float-time (or (current-idle-time)
                     '(0 0 0)))))

(defun my-erc-page-popup-notification (nick)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory "/home/klaes/"))
      ;; 8640000 milliseconds = 1 day
      (start-process "page-me" nil "notify-send"
                     "-u" "normal" "-t" "2000" "ERC"
                     (format my-erc-page-message nick)))))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
 If DELAY is specified, it will be the minimum time in seconds
 that can occur between two notifications.  The default is
 `my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (float-time (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
 matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (my-emacs-is-idle)
             ;; Dont show notifications if already typing in the convo
             ;;(away-p) ;; it dudnt fecking w0rk
             (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (null (string-match "\\`\\([sS]erver\\|localhost\\)" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick)
             ;; or from blist calls
             (null (string-match "\\(localhost\\|root\\)" nick)))
    (my-erc-page-popup-notification nick)))
(add-hook 'erc-text-matched-hook 'my-erc-page-me)

(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
      (my-erc-page-popup-notification nick)
      nil)))
(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)


;; Make tray icon do something when people write on ERC
(require 'erc)
(require 'notifications)
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (unless (my-emacs-is-idle my-erc-page-timeout)
    (notifications-notify
     :title nick
     :body message
     :app-icon "/var/abs/local/yaourtbuild/notify-osd-better-git/src/notify-osd-better/data/icons/scalable/notification-message-im.svg"
     :urgency 'low)))

(add-hook 'erc-text-matched-hook 'erc-global-notify)


;; Set emacs to open some browser by default
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;; Get bitlbee to show different colours for people online/offline, etc
(erc-match-mode 1)
(setq erc-keywords '((".*Online.*" (:foreground "green"))
                     (".*Busy" (:foreground "red"))
                     (".*Away" (:foreground "red"))
                     (".*Idle" (:foreground "orange"))
                     ("klaes *[,:;]" "\\bklaes[!?.]+$" "hey klaes")
                     ))

;; LaTeX code in erc
(require 'erc-tex)

;; Save logs of chats
(setq erc-log-channels-directory "/home/klaes/.emacs.d/logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

;; Write to the last person I wrote to again
                                        ;(setq bitlbee-target "")
                                        ;(defun bitlbee-update-target (msg)
                                        ;  (if (string-match "\\([^:]*: \\)" msg)
                                        ;      (setq bitlbee-target (match-string 1 msg))
                                        ;    (if (not (or
                                        ;              (string-match "account" msg)
                                        ;              (string-match "help" msg)
                                        ;              (string-match "identify" msg)
                                        ;              (string-match "blist" msg)))
                                        ;        (setq str (concat bitlbee-target msg)))))
                                        ;(add-hook 'erc-send-pre-hook 'bitlbee-update-target)

;; Stop the silly bell
(setq ring-bell-function 'ignore)
