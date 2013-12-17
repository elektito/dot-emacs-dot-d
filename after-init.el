;;; themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'zenburn t)

;;; fullscreen support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; toggle full-screen mode
(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))

;; bind it to f11 key.
(global-set-key [f11] 'fullscreen)

(add-hook 'after-make-frame-functions 'fullscreen)

;;; backup file management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; put backup files (i.e., `foo~' or `foo.~i~') in one place. LIFE SAVER!
;;
;; regexp => directory mappings
;; filenames matching a regexp are backed up in the corresponding directory
(setq backup-directory-alist
      '((".*" . "~/.emacs_backups/")))  ;; '(("." . "~/.saves"))
      ;; Emacs will `make-directory' it if necessary

;;; region selection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable highlighted region selection.
(transient-mark-mode 0)

;;; visual clutter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable menu bar, tool bar and scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; rebind menu-bar-open to f12 so that it won't conflict with unity's
;; f10 key binding
(global-set-key [f12] 'menu-bar-open)

;;; mode bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(line-number-mode t)
(column-number-mode t)

;;; programming styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable auto-complete mode
(require 'auto-complete)
(global-auto-complete-mode t)

;; Create my C/C++ personal style.
(defun my-c-mode-hook ()
  (flyspell-prog-mode)
  (setq c-basic-offset 2))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (flyspell-prog-mode)
  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; bind RET to py-newline-and-indent
(defun my-python-mode-hook ()
  (flyspell-prog-mode)
  ;; NOTE: epc is needed for jedi: pip install epc
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)

  (setq python-indent-offset 2)
  (define-key python-mode-map "\C-m" 'newline-and-indent))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; automatically go to python-mode for wscript files (waf scripts)
(setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))

(defun my-emacs-lisp-mode-hook ()
  (flyspell-prog-mode))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; qt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'qt-pro)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;;; setup slime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/source/slime/")
(require 'slime)
(slime-setup '(slime-repl slime-fuzzy))

;;; shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make shell prompt read-only
(setq comint-prompt-read-only t)

;;;; w3m ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; keyboard short-cuts
(global-set-key "\C-xm" 'browse-url-at-point)
(global-set-key "\C-xn" 'w3m-browse-url)

;; enable cookies
(setq w3m-use-cookies t)

(eval-after-load 'w3m
  '(progn
     (define-key w3m-mode-map "," 'w3m-previous-buffer)
     (define-key w3m-mode-map "." 'w3m-next-buffer)
     (define-key w3m-mode-map "x" 'w3m-goto-url-new-session)
     (define-key w3m-mode-map "z" 'w3m-view-this-url-new-session)))nil

(require 'w3m-search)

;; add wikipedia to the list of search engines
(add-to-list
 'w3m-search-engine-alist
 '("wikipedia-en"
   "http://en.wikipedia.org/wiki/Special:Search?search=%s"))

;; add 'wp:' prefix for searching wikipedia.
(add-to-list 'w3m-uri-replace-alist
             '("\\`wp:" w3m-search-uri-replace "wikipedia-en"))

;; set wikipedia as the default search engine.
(setq w3m-search-default-engine "wikipedia-en")

;; define a function and a key binding to switch between w3m and other
;; buffers.
(defun wicked/toggle-w3m ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      ;; Currently in a w3m buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'w3m-mode)
        (bury-buffer))
    ;; Not in w3m
    ;; Find the first w3m buffer
    (let ((list (buffer-list)))
      (while list
        (if (with-current-buffer (car list)
              (derived-mode-p 'w3m-mode))
            (progn
              (switch-to-buffer (car list))
              (setq list nil))
          (setq list (cdr list))))
      (unless (derived-mode-p 'w3m-mode)
        (call-interactively 'w3m)))))

(global-set-key [f7] 'wicked/toggle-w3m)

;;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t) ;; enable flx-ido which results in fuzzy matching
		 ;; with good sorting
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights

;; initiate garbage collection every 20MB (higher than the default
;; value). this can make some operations faster in expense of memory.
(setq gc-cons-threshold 20000000)

;; set the modes for which flyspell mode is enabled/disabled
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; set a separate custom file so that this file is not modified by
;; Customize.
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)
