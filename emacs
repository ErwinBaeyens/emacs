; .emacs

;; disable the initial splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

 ;; show the size of the file in kb, mb...
(size-indication-mode t)

;; show linenumbers
(setq line-number-mode t)

;; show the column number in the status bar
(setq column-number-mode t)

;; syntax highlighting related
(setq font-lock-maximum-decoration t)
(setq global-font-lock-mode t)

;; match parentheses
(setq show-paren-mode t)

;; save minibuffer history
(setq savehist-mode t)

;; When running on Unix and X handle correct clipboard functionality
(setq x-select-enable-clipboard t)

(setq set-scroll-bar-mode 'right)

;; display the time in the status line
(setq display-time-mode t)

;; alias yes and no to y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; set the fill column
(setq-default fill-column 100)

;; indicate empty lines
(setq indicate-empty-lines t)

;; add a personal lisp dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

(custom-set-variables
 ;; custom-set-variables was adde by Custom.
 ;; If you add it by hand, you could mess it up, so be carefull
 ;; your init file should contain only one such instance,
 ;; if there is more than one, they won't work right.
 '(custom-enabled-themes (quoute (tsdh-dark)))
 '(diff-switches "-u"))

;; load a theme
(load-theme 'wombat t)


(tool-bar-mode -1)
(menu-bar-mode 1)



(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives'("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)


;; ;(custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;
;; startup using fullscreen
;;
(defun toggle-fullscreen()
  " Toggle fullscreen on X11"
  (interactive)
  (when ( eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil-'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

(defun fullscreen ( )
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; find-file-root
;; this lets you open files with the root credentials
;;
(defvar find-file-root-prefix
  (if (featurep 'xemax)"/[sudo/root@localhost]" "/sudo:root@localhost:")
  "*The filename prefix used to open a file with`find-file-root'")

(defvar find-file-root-history nil
  "History list for file found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root()
  "*Find a file as the root user. Prepends`find-file-root-prefix'
  to the selected file nameso that it may be accessed via the
  corresponding tramp method."
  (interactive)
  (require 'tramp)
  (let* (;; we bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for root files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)
	; if called from a root file,  we need to fix up the path .
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir(file-name-directory path)))

    (when (setq file (read-file-name "Find file(UID = 0): "dir path))
      (find-file (concat find-file-root-prefix file))
      ;; if this all succeeded save our new historylist.
      (setq find-file-root-history file-name-history)
      ;; allow some user customisation
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x)(control t)] 'find-file-root)

(defun backward-kill-line (arg)
  "Kill line Backwards."
  (interactive "p")
  (kill-line (- 1 arg)))

(global-set-key "\C-c\C-c" 'backward-kill-line) ;;'C-cC-c'
(global-auto-complete-mode t)

;; activate Which-emacs
(require 'which-key)
(which-key-mode)


;;
;; insert the date of today
;;

(require 'calendar)

;; defun insdate-insert-current-date
;; &optional omit-day-of-week-p

(defun insdate-insert-current-date(&optional omit-day-of-week-p)
  "insert today's date using the current locale.
With a prefix argument, the date is inserted without the day of
the week."
  (interactive "P*")
  (insert(calendar-date-string (calendar-current-date)nil
			       omit-day-of-week-p)))

(global-set-key "\C-x\M-d" 'insdate-insert-current-date)


(add-to-list 'load-path "/usr/share/emacs/site-lisp/git")
(add-to-list 'vc-handled-backends 'GIT)
(autoload 'git-status "git" "Entry point in to git-status mode." t)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for GIT." t)

;; Key bindings for git
(global-set-key "\C-xgs" 'git-status)

;; all blank lines except single ones
(defun single-lines-only()
  "replace multiple blanklines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)
    )
  )


;;
;; provisions for iedit-mode this allows for multiple edits in a file
;; like changing the name of a variabel in one go in the entire file
;;

(add-to-list 'load-path (expand-file-name "~erwin/.emacs.d/lisp/iedit"))
(require 'iedit)

(defun iedit-dwim (arg)
  "starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
	(widen)
	;; tusi function deternines the scope of iedti-start.
	(if iedit-mode
	    (iedit-done)
	  ;;current word can be replaced by any
	  ;; other function
	  (narrow-to-defun)
	  (iedit-start (current-word)(point-min)(point-max)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Begin python-mode related settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/python-mode")
(setq py-install-directory "~/.emacs.d/lisp/python-mode")

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python -mode))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End of python related settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path  "~/.pymacs"))

(when (load "flymake" t)
  (defun flymake-pyflakes-init()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)


(defun my-flymake-show-help()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help(get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

