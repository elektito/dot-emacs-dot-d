;;; -*-  indent-tabs-mode: nil;  -*-

(require 'cl)
(require 'package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;;; list of packages to obtain if not installed
(setq my-packages
      '(ido
        flx-ido
        flyspell
        multi-term
        w3m
        smex

        ;; programming
        markdown-mode
        auto-complete
        jedi
        yasnippet
        autopair
        glsl-mode
        protobuf-mode
        rainbow-delimiters
        whitespace-cleanup-mode
        yaml-mode
        dockerfile-mode
        go-mode
        dart-mode

        ;; themes
        zenburn-theme))

(package-initialize)

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p my-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))

;;; make sure el-get is installed
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes/")
(add-to-list 'el-get-sources 'yasnippet-snippets)
(setq my-packages 'yasnippet-snippets)
(el-get 'sync my-packages)

;;; load the real init file after all elpa packages have been loaded
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/after-init.el")))
