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

	;; programming
	markdown-mode
	auto-complete
	jedi
	yasnippet-bundle
        autopair

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

;;; load the real init file after all elpa packages have been loaded
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/after-init.el")))
