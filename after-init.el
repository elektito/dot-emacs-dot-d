;;; -*-  indent-tabs-mode: nil;  -*-

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

;;; visual clutter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable menu bar, tool bar and scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; disable highlighted region selection.
(transient-mark-mode 0)

;; disable startup message
(setq inhibit-startup-message t)

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
  (c-set-offset 'innamespace 0)
  (define-key c++-mode-map "\C-m" 'newline-and-indent))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Some workarounds for C++11 code. From this Stack Overflow answer:
;; http://stackoverflow.com/a/12934513/363949
(require 'font-lock)

(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(add-hook
 'c++-mode-hook
 '(lambda()
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; add the new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT
           ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; user-types (customize!)
           ;;("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
        ) t)

;;; bind RET to py-newline-and-indent
(defun my-python-mode-hook ()
  (flyspell-prog-mode)
  ;; NOTE: epc is needed for jedi: pip install epc
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)

  (setq python-indent-offset 4)
  (define-key python-mode-map "\C-m" 'newline-and-indent))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; automatically go to python-mode for wscript files (waf scripts)
(setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))

(defun my-emacs-lisp-mode-hook ()
  (flyspell-prog-mode))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; enable auto-pairing braces
(require 'autopair)
(autopair-global-mode)

;; qt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'qt-pro)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;;; setup slime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/source/slime/")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make shell prompt read-only
(setq comint-prompt-read-only t)

;;;; w3m ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; flyspell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set the modes for which flyspell mode is enabled/disabled
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;; email and news ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a ~/.authinfo file is needed with the following content:
;;
;; machine imap.googlemail.com login mostafa@sepent.com password secret port 993
;; machine smtp.googlemail.com login mostafa@sepent.com password secret port 465
;;
;; where 'secret' is the password.

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.googlemail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.googlemail.com" 465 nil nil))
      smtpmail-auth-credentials '(("smtp.googlemail.com" 465
                                   "mostafa@sepent.com" nil))
      smtpmail-default-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 465
      smtpmail-local-domain "sepent.com"
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;;; whitespace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't mix tabs and spaces
(setq-default indent-tabs-mode nil)

;; enable yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/el-get/yasnippet-snippets"))
(yas-global-mode 1)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t) ;; enable flx-ido which results in fuzzy matching
                 ;; with good sorting
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights

;; rebind menu-bar-open to f12 so that it won't conflict with unity's
;; f10 key binding
(global-set-key [f12] 'menu-bar-open)

;; initiate garbage collection every 20MB (higher than the default
;; value). this can make some operations faster in expense of memory.
(setq gc-cons-threshold 20000000)

;; set a separate custom file so that my files are not modified by
;; Customize.
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

;; start emacs server
(server-start)
