;;
;; File .emacs - These commands are executed when GNU emacs starts up.
;;
;; $Id: .emacs,v 1.8 1995/11/07 20:12:07 dewell Exp $
;; revised 8/15/2009
;;
;; Now, it resides as .emacs.d/init.el

;; Keep Emacs from executing file local variables.
;; (this is also in the site-init.el file loaded at emacs dump time.)
(setq inhibit-local-variables t  ; v18
      enable-local-variables nil ; v19
      enable-local-eval nil)     ; v19

;; Swap Backspace and Delete keys, except for v19 running under X.  This works
;; on both HPs and Suns.
(or (and (eq window-system 'x)
         (string-match "\\`19\\." emacs-version))
    (load "term/bobcat"))

;; Cause the region to be highlighted and prevent region-based commands
;; from running when the mark isn't active.
 
(pending-delete-mode t)
 (setq transient-mark-mode t)

;(setq kill-emacs-query-functions
;  (list (function (lambda ()
;                    (ding)
;                    (y-or-n-p "Er du nu lige helt sikker hva? Kunne du ikke bare sige til hvis du virkelig ville ud? ")))))

;; Fonts are automatically highlighted.  For more information
;; type M-x describe-mode font-lock-mode 

(global-font-lock-mode t)

;; "rmail" is the standard Emacs mail reading mode if you want try a
;; different one then "vm" works well
;;
;; VM mail reading mode
(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
;;
;; win-vm window+menus for VM (Use the above 5 autoloads or the following,
;;                             but not both.)
;;(let ((my-vm-pkg
;;       (if (not window-system)
;;	   "vm"
;;	 (define-key menu-bar-file-menu [rmail] '("Read Mail" . vm))
;;	 (define-key-after menu-bar-file-menu [smail]
;;	   '("Send Mail" . vm-mail) 'rmail)
;;	 "win-vm")))
;;  (autoload 'vm my-vm-pkg "Read and send mail with View Mail." t)
;;  (autoload 'vm-mode my-vm-pkg "Read and send mail with View Mail." t)
;;  (autoload 'vm-mail my-vm-pkg "Send mail with View Mail." t)
;;  (autoload 'vm-visit-folder my-vm-pkg))

;; Some color stuff if you want it.
;; (cond (window-system
;;        (setq hilit-mode-enable-list  '(not text-mode)
;;              hilit-background-mode   'light
;;              hilit-inhibit-hooks     nil
;;              hilit-inhibit-rebinding nil)
;; 
;;        (require 'hilit19)
;;        ))
;; 
;; Example of how to set the highlighting of color defaults.
;; (if (fboundp 'set-face-background)
;;     (progn
;;      (set-face-background (quote highlight) "yellow")
;;      (set-face-foreground (quote highlight) "black")))


;; Below are changes taken from the tutor .emacs file
;; Added by Craig Ruefenacht

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This provides customized support for writing programs in different kinds
;;;; of programming languages.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (append '(("\\.C\\'" . c++-mode)
                ("\\.cc\\'" . c++-mode)
		("\\.c\\'" . c-mode)
                ("\\.h\\'"  . c++-mode))
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
    (c++-mode     . "g++ -g %s -o %s -lm")	; Doesn't work for ".h" files.
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
    (compile-internal check-command "Can't find next/previous error"
		      "Checking" nil nil nil)))

(defun make ()
  "Run make in the directory of the file contained in the current buffer"
  (interactive)
  (save-some-buffers nil nil)
  (compile-internal (read-from-minibuffer "Make command: " "make ")
		    "Can't find next/previous error" "Make"
		    nil nil nil))


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
	      '(("\\.C$"   . c++-mode))
	      '(("\\.cc$"  . c++-mode))
	      '(("\\.icc$" . c++-mode))
	      '(("\\.c$"   . c-mode))
	      '(("\\.y$"   . c-mode))
	      '(("\\.h$"   . c++-mode))
	      auto-mode-alist))
;;
;; Finally look for .customs.emacs file and load it if found

(if "~/.customs.emacs" 
    (load "~/.customs.emacs" t t))

;; Art: added with v. 23.1 to make spacebar complete filenames (8/17/2009)
(progn
 (define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)
 (define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
 (define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)) 

;; Art: added with v. 23.1
;; Set env variable this way?  I used the traditional way instead
;(info "(emacs) Windows HOME")

;; End of file.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-battery-mode t)
 '(display-time-mode t)
 '(menu-bar-mode t)
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit (default family consolas) :stipple nil :background "black" :foreground "gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "fixed")))))

;; Make emacs load maximized under windows.
(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))
 
(add-hook 'window-setup-hook 'w32-maximize-frame t)


;; My own settings:
;Compiler med pdflatex
(setq TeX-PDF-mode t)

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
    (if (file-exists-p "~/.emacs")
    (load-file "~/.emacs")))

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
(menu-bar-mode 0)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set emacs to reload last used buffers
(desktop-save-mode 1)

(setq linum-format "%d ")

(linum-mode 1)

(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

