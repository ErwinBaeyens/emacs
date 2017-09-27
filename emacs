;; -*- mode: lisp;  fill-column: 75; comment-column: 50; -*-
;; .emacs.el

(if (display-graphic-p)
    (progn
      ;; When running on Unix and X handle correct clipboard functionality
      (setq x-select-enable-clppipboard t)
      (set-scroll-bar-mode 'right))
  )

;; disable the initial splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(tool-bar-mode 0)

;; setup the initial scratch file
(setq initial-major-mode 'python-mode)
(setq initial-scratch-message nil)

 ;; show the size of the file in kb, mb...
(size-indication-mode t)

;; show line numbers
(global-linum-mode t)

;; show the column number in the status bar
(column-number-mode t)

;; syntax highlighting related
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; match parentheses
(show-paren-mode t)
(electric-pair-mode t)

;; save minibuffer history
(savehist-mode t)

;; display the time in status line
(display-time-mode t)

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; set the fill column
(setq-default fill-column 80)

;; show trailing whitespace
(setq show-trailing-whitespace 1)

;; no tabs
(setq-default indent-tabs-mode nil)

;; add a personal lisp dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; load a theme
(load-theme 'wombat)

(defun my-split-window-func ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key "\C-x2" 'my-split-window-func)


;; install melpa mode
;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))

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
(setq python-shell-interpreter '/usr/bin/python) 
(add-to-list 'load-path "~/.emacs.d/lisp/python-mode") 
(setq py-install-directory "~/.emacs.d/lisp/python-mode")

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
(add-hook 'python-mode-hook 'electric-pair-mode )
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

;; (require 'run-current-file)
(defun run-current-file ()
  "Execute the current file. For example, if the current buffer 
   is the file x.py, then it'll call 「python x.py」 in a shell.
   The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, 
   Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
   File suffix is used to determine what program to run.

   If the file is modified or not saved, save it automatically before run.

   URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
   version 2016-01-28"
  (interactive)
  (let (
         (-suffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("go" . "go run")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("rkt" . "racket")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")
            ("java" . "javac")
            ))

         -fname
         -fSuffix
         -prog-name
         -cmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))

    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (shell-command -cmd-str "*run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (shell-command -cmd-str "*run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(global-set-key (kbd "<f8>") 'run-current-file)

(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;;;org-mode configuration
;; enable org-mode
(require 'org)
;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswithchb)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; enable auto-fill-mode in latex mode
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; enable auto-fill-mode in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; php-mode
;; php autocomplete 
(add-hook 'php-mode-hook '
          (lambda ()
            (auto-complete-mode t)
            (require' ac-php)
            (setq ac-sources 'ac-source-php))
          (yas-global-mode 1))

;; yaml-mode 
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda()
          (define-key yaml-mode-map "C-m" 'newline-and-indent)))
(add-hook 'yaml-mode-hook
          '(lambda()
             (auto-complete-mode t)
             (setq ac-yaml 'ac-source-yaml)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(diff-switches "-u")
 '(org-agenda-files (quote ("~/1.org")))
 '(package-selected-packages
   (quote
    (nlinum ac-php php-mode org org-gnome vala-mode ## bison-mode yaml-mode js2-mode auto-complete-exuberant-ctags ac-etags)))
 '(safe-local-variable-values (quote ((conding . utf-8))))
 '(send-mail-function (quote mailclient-send-it)))
