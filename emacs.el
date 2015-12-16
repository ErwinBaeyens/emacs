; .emacs

;; disable the initial splash screen
(setq inhibit-splash-screen t)

;; show the size of the file in kb, mb...
(size-indication-mode t)

;; show line numbers
(global-linum-mode 1)

;; show the column number in the status bar
(column-number-mode t)

;; syntax highlighting related
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; match parentheses
(show-paren-mode t)

;; save minibuffer history
(savehist-mode t)

;; When running on Unix and X handle correct clipboard functionality
(setq x-select-enable-clipboard t)

(set-scroll-bar-mode 'right)

;; display the time in status line
(display-time-mode t)

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; set the fill column
(setq-default fill-column 80)

;; indicate empty lines
(setq indicate-empty-lines t)

;; add a personal lisp dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(diff-switches "-u"))

;; load a theme
(load-theme 'wombat)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(defun java-mode-untabify()  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
           (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max))))
  nil)

(add-hook 'java-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hook)
             (add-hook 'write-contents-hook 'java-mode-untabify)))

(defun my-split-window-func ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key "\C-x2" 'my-split-window-func)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

					;
;; install melpa mode
;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b - %f")

;; highlight incremental search
(setq search-highlight t)


(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'load-path "~/.emacs.d/melpa/auto-complete-1.5.0")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs.d/auto-complete-1.5.0/dict")
;;(setq-
 
(add-hook 'js2-mode-hook 'ac-js2-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Begin python-mode related settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp/python-mode") 
(setq py-install-directory "~/.emacs.d/lisp/python-mode")

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(add-hook 'python-mode-hook 'imenu-add-menubar-index)
(global-set-key [mouse-3] 'imenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End python-mode related settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(global-auto-complete-mode t)

;; mic-paren mode for advanced parantheses matching
(require 'mic-paren) ; loading
(paren-activate)     ; activating


;; find-file-root 
(defvar find-file-root-prefix (if
	(featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
	"*The filename prefix used to open a file with `find-file-root'")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for fuctions to run after finding a \"root\" file.")

(defun find-file-root()
  "*Open a file as the root user.
  Prepends `find-file-root-prefix' to the selected file name so that it 
  may be accessed via the corresponding tramp mehod."

  (interactive)
  (require 'tramp)
  (let* (;; we bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for, "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)
   ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find fike(UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
    (setq find-file-root-history file-name-history)
    ;; allow some user customisation
    (run-hooks 'find-file-root-hook))))


(global-set-key [(control x) (control t)] 'find-file-root)

;; find aspell and hunspell automatically
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
))
 ((executable "hunspell")
  (setq ispel-program-name "hunspell")
  (setq ispell-extra-args '("-d en_GB")))
)
