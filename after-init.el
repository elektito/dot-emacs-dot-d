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
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

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

;; hook for all programming modes
(defun my-prog-mode-hook ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; customize emacs lisp mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)))

;; Create my C/C++ personal style.
(defun my-cc-mode-hook ()
  (flyspell-prog-mode)
  (setq c-basic-offset 2)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my-cc-mode-hook)

(defun my-c++-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0))
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

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4)
  (setq indent-tabs-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)


(defun my-dart-mode-hook ()
  (add-hook 'before-save-hook 'dartfmt-before-save)
  (setq dart-sdk-path "/home/mostafa/flutter/flutter/bin/cache/dart-sdk/")
  (setq tab-width 2)
  (setq indent-tabs-mode 1))
(add-hook 'dart-mode-hook 'my-dart-mode-hook)

;; enable rainbow-delimiters mode for all programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

;; enable glsl mode for .glsl, .vert and .frag file extensions
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; enable groovy-mode for Jenkinsfiles.
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

;; navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move to the matching brace by pressing %. If not on a brace, simply
;; insert a %.
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

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
     (define-key w3m-mode-map "\C-ck" 'w3m-delete-buffer)  ; close tab
     (define-key w3m-mode-map "," 'w3m-previous-buffer)
     (define-key w3m-mode-map "." 'w3m-next-buffer)
     (define-key w3m-mode-map "x" 'w3m-goto-url-new-session)
     (define-key w3m-mode-map "z" 'w3m-view-this-url-new-session)))

(require 'w3m-search)

;; add wikipedia to the list of search engines
(add-to-list
 'w3m-search-engine-alist
 '("wikipedia-en"
   "http://en.wikipedia.org/wiki/Special:Search?search=%s"))

(add-to-list
 'w3m-search-engine-alist
 '("google"
   "http://www.google.com/search?q=%s&pws=0&gl=us&gws_rd=cr"))

;; add 'wp:' prefix for searching wikipedia.
(add-to-list 'w3m-uri-replace-alist
             '("\\`wp:" w3m-search-uri-replace "wikipedia-en"))

(add-to-list 'w3m-uri-replace-alist
             '("\\`gg:" w3m-search-uri-replace "google"))

;; set wikipedia as the default search engine.
(setq w3m-search-default-engine "google")

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

(defun my-whitespace-cleanup ()
  (when (not (derived-mode-p 'makefile-mode))
    (whitespace-cleanup)))

;; use whitespace-cleanup-mode instead of adding `whitespace-cleanup`
;; to the `before-save-hook`. This cleans up the code when saving on
;; the condition that it was initially clean. This way when we edit
;; already broken (by our standards at least!) code, we won't mess up
;; the entire file
(global-whitespace-cleanup-mode)

;;; org mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; archive all DONE tasks with "C-c A".
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))
(global-set-key "\C-cA" 'my-org-archive-done-tasks)

;;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't mix tabs and spaces
(setq-default indent-tabs-mode nil)

;; enable yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/el-get/yasnippet-snippets"))
(yas-global-mode 1)

; lately, yasnippet has been interfering with term-mode (causing tab
; auto-completion not work) when emacs is running in a window. this
; fixes that problem.
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)))

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t) ;; enable flx-ido which results in fuzzy matching
                 ;; with good sorting
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights

;; Set M-x to use smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; rebind menu-bar-open to f12 so that it won't conflict with unity's
;; f10 key binding
(global-set-key [f12] 'menu-bar-open)

;; scroll other window up when C-M-V (that is, control-alt-shift-v) is
;; pressed
(defun scroll-other-window-up (&optional f)
  (interactive)
  (scroll-other-window '-))
(global-set-key "\M-\C-V" `scroll-other-window-up)

;; initiate garbage collection every 20MB (higher than the default
;; value). this can make some operations faster in expense of memory.
(setq gc-cons-threshold 20000000)

;; set a separate custom file so that my files are not modified by
;; Customize.
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

;; start emacs server
(server-start)
