;; -*- mode: lisp;  fill-column: 75; comment-column: 50; -*-
;; .emacs.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color)
(set-cursor-color "Cornflower blue")
(setq default-frame-alist '((cursor-color . "Cornflower blue")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (display-graphic-p)
    (progn
      ;; When running on Unix and X handle correct clipboard functionality
      (setq x-select-enable-clipboard t)
      (set-scroll-bar-mode 'right))
  )

;; get the right ssh key


;; disable the initial splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(tool-bar-mode 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Erwin Baeyens")
(setq user-mail-address "Erwin.Baeyens@belgiantrain.be")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load a theme
(load-theme 'wombat)


;; show the size of the file in kb, mb...
(size-indication-mode t)

;; show line numbers
(global-linum-mode t)


;; setup the initial scratch file
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

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

;; make dired sort with directories on top
(setq dired-listing-switches "-al --group-directories-first")

;; no tabs
(setq-default indent-tabs-mode nil)

;; set up the  load-path
(let ((default-directory  "~/.emacs.d/lisp"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path'(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

;; =============================================================================
;; window and user interface functions
;; =============================================================================
(defun my-split-window-func ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer))
  )

(global-set-key "\C-x2" 'my-split-window-func)


(defun swap-windows()
  "If you have 2 windows it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "you need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  )

(global-set-key [C-tab] 'swap-windows)

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b - %f")

;; highlight incremental search
(setq search-highlight t)

;; =============================================================================
;; End of window and user interface functions 
;; =============================================================================


;; =============================================================================
;; install melpa mode
;; =============================================================================
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/elpa/auto-complete*")


;; =============================================================================
;; mic-paren mode for advanced parantheses matching
;; =============================================================================
(require 'mic-paren) ; loading
(paren-activate)     ; activating


;; =============================================================================
;;
;;  use shackle.  Shackle allows for setting rules how buffers should open in
;;  new windows or reuse windows.
;;
;; =============================================================================
(require 'use-package)
(use-package shackle
  :hook
  (after-init . shackle-mode)
  :custom
  (shackle-rules '((help-mode :inhibit-window-quit t :same t)))
  (shackle-select-reused-windows t))


;; =============================================================================
;;
;; Begin python-mode related settings
;;
;; =============================================================================

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq python-shell-interpreter '/usr/bin/python)
(add-to-list 'load-path "~/.emacs.d/lisp/python-mode")
(setq py-install-directory "~/.emacs.d/lisp/python-mode")


;; ;; enable flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;; =============================================================================
;; = Jedi mode for code completion
;; =============================================================================
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;; =============================================================================
;; = End python-mode related settings
;; =============================================================================

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs.d/auto-complete-1.5.0/dict")
(add-hook 'js2-mode-hook 'ac-js2-mode)


(require 'auto-complete)
(global-auto-complete-mode t)


;; =============================================================================
;; mic-paren mode for advanced parantheses matching
;; =============================================================================
(require 'mic-paren) ; loading
(paren-activate)     ; activating


;; =============================================================================
;; = find-file-root
;; =============================================================================

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

    (when (setq file (read-file-name "Find file(UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customisation
      (run-hooks 'find-file-root-hook))))


(global-set-key (kbd "C-x C-t") 'find-file-root)

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
           ;; ("php" . "php")
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

(defun insert-current-time()
  "insert the current weekday and time at point."
  (interactive)
  (format-time-string "%z")
  (current-time-format "%z")
  (insert (format-time-string current-time-format(current-time))))

(defun insert-current-date-time()
  "insert the current date and time at point."
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))


(global-set-key "\C-cd" 'insert-current-date-time)
(global-set-key "\C-ct" 'insert-current-time)


;; ;; =============================================================================
;; ;; make whitespace searches match exactly what you type
;; ;; =============================================================================
;; (setq search-whitespace-regexp nil)

;; org-mode configuration
;; enable org-mode

(require 'org)
(require 'ox-asciidoc)
(require 'ox-md)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswithchb)
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


;; set up the different agenda files.
(setq org-agenda-files (list "~/Dropbox/Erwin Stuff/todo/work.org"
                             "~/Dropbox/Erwin Stuff/todo/home.org"
                             "~/Dropbox/Erwin Stuff/todo/private.org"))


(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~Dropbox/Erwin Stuff/todo/tasks.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))


(defun org-babel-execute:yaml (body params) body)


;; enable auto-fill-mode in latex mode
(add-hook 'latex-mode-hook 'turn-on-auto-fill)


;; enable auto-fill-mode in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'yaml-mode-hook
          '(lambda()
             (auto-complete-mode t)
             (setq ac-yaml 'ac-source-yaml)))


;; =============================================================================
;; Multiple cursor setup
;; =============================================================================
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)


;; =============================================================================
;; magit stuff
;; =============================================================================
(global-set-key (kbd "C-x g") 'magit-status)
;; =============================================================================
;; end magit stuff
;; =============================================================================

(load-library "hideshow")
(global-set-key (kbd "C-'") 'toggle-hiding)
(global-set-key (kbd "C-=") 'toggle-selective-display)

;; (add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)
(add-hook 'web-mode-hook 'hs-minor-mode)


;; =============================================================================
;; enable web-mode for all associated files
;; =============================================================================
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for webmode."
  (setq-default web-mode-enable-auto-indentation t)
  (setq-default web-mode-markup-indent-offset 2 )  ;; html offset
  (setq-default web-mode-css-indent-offset 2 )     ;; css offset
  (setq-default web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'web-mode-hook)

;; =============================================================================
;; end web-mode
;; =============================================================================

(defun calculate-test-result()
  "Calculate test result.
This function assumes that you use Y for correct and N for wrong answers"
  (interactive)
  (let* ((correct (how-many "[yY]\$" 1))
         (wrong (how-many "[nNX]\$" 1))
         (total (+ correct wrong))
         (percentage (/ (* 100.0 correct)
                        total)))
    (message (concat "Your test score percentage: "
                     (number-to-string percentage)))))

(global-set-key (kbd "<f12>") 'calculate-test-result)


;; =============================================================================
;; just a try for c-mode
;; =============================================================================
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda()
            (let ((filename (buffer-file-name)))
              ;; Enable Kernelmode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

(require 'wgrep)

;; =============================================================================
;; ibuffer mode
;; =============================================================================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-group
      '("home"
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . ".emacs")
                             (filename . ".init.el")
                             ))
       (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("programming" (or
                               (mode . python-mode)
                               (mode . c++-mode)
                               (mode . yaml-mode)))
               ("emacs" (name . "^\\*scratch\\*$")))
              )))


(add-hook 'ibuffer-mode-hook
          (lambda()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Don't show filter groups if they are empty
(setq ibuffer-show-empty-filter-groups nil)

;; Don ask for confirmation on every step
(setq ibuffer-expert t)


;; =============================================================================
;; This is for automatically generating the PDF file for the current AsciiDoc
;; that you are editing. It adds a new hook (function) to be automatically run
;; after each save action in `adoc-mode'
;;
;; if you enable `auto-revert-mode' in your PDF viewing mode, then you will have
;; Live Preview functionallity of your AsciiDoc document
;;
;; The following code assumes the existence of the `asciidoc' package. For more
;; details, please see http://asciidoc.org/
;; =============================================================================
(defun run-a2x-to-generate-pdf ()
  "generate the PDF representation of the current file."
  (when(eq major-mode 'adoc-mode)
    (shell-command-to-string (format "a2x -f pdf %s" buffer-file-name))))

(add-hook 'after-save-hook #'run-a2x-to-generate-pdf)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("6876d0eb1bcef5ce6a71434d7c19a66a1a14378e22dce3b72b1fbc51aeb5e18d" default)))
 '(display-time-mode t)
 '(eudc-default-return-attributes (quote all))
 '(eudc-server-hotlist (quote (("" . bbdb) ("ldap://pro-ipa-blo.psop.be" . ldap))))
 '(eudc-use-raw-directory-names t)
 '(font-use-system-font t)
 '(markdown-command "/usr/bin/pandoc")
 '(org-export-with-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
    (aggressive-fill-paragraph aggressive-indent vterm autotetris-mode shackle wgrep-ag ag helm-swoop org-link-minor-mode md-readme ascii-art-to-unicode pyenv-mode ace-window projectile markdown-mode+ markdown-preview-eww markdown-preview-mode markdown-toc markdownfmt markdown-mode dired-subtree adoc-mode ox-asciidoc ## keychain-environment yafolding flymake-vala vala-mode vala-snippets highlight-indent-guides async dash ghub git-commit graphql treepy with-editor magit-popup ac-etags ac-html ac-html-angular ac-html-csswatcher ac-php ac-php-core ac-rtags auto-complet auto-complete-exuberant-ctags bison-mode counsel datetime datetime-format flycheck flycheck-phpstan flycheck-pycheckers flycheck-pyflakes flymake-json flymake-php flymake-python-pyflakes flymake-yaml gandalf-theme icicles jedi jinja2-mode js2-mode json-mode load-dir magit mic-paren multiple-cursors nlinum org org-gnome powershell python python-mode speed-type ssh ssh-agency ssh-config-mode ssh-deploy ssh-tunnels swiper swiper-helm tile time-ext typing web-mode yaml-mode yasnippet)))
 '(reb-re-syntax (quote read))
 '(safe-local-variable-values (quote ((conding . utf-8))))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(sql-mysql-login-params (quote (user password server database port)))
 '(sql-mysql-program "mysql --protocol=tcp")
 '(tool-bar-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For documentation, see https://github.com//DarthFennec.highlight-indent-guides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)
(setq highlight-indent-guides-responsive 'stack)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal)))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
