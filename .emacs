;; predictive install location
(add-to-list 'load-path "~/.emacs.d/predictive/")
;; dictionary locations
(add-to-list 'load-path "~/.emacs.d/predictive/latex/")
(add-to-list 'load-path "~/.emacs.d/predictive/texinfo/")
(add-to-list 'load-path "~/.emacs.d/predictive/html/")
;; load predictive package
(autoload 'predictive-mode "~/.emacs.d/predictive/predictive"
  "Turn on Predictive Completion Mode." t)

;Compiler med pdflatex
(setq TeX-PDF-mode t)

;Fjern statup msg!!!
(setq inhibit-startup-message t)

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

;; Show column numbers
(setq column-number-mode t)
 
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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit (default family consolas) :stipple nil :background "black" :foreground "gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "fixed")))))

