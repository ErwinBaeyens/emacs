;; -*- Mode: lisp;  fill-column: 75; comment-column: 50; lexical-binding: t; -*-
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
;; (menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Erwin Baeyens")
(setq user-mail-address "Erwin.Baeyens@benerail.com")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load a theme
(load-theme 'wombat)

;; show the size of the file in kb, mb...
(setq size-indication-mode t)

;; show line numbers
(if (version< emacs-version "26.1")
    (progn (global-linun-mode t))
    (progn (global-display-line-numbers-mode t))
    )
;; (global-display-line-numbers-mode t)

;; change the look of the linenubers
(add-hook 'display-line-numbers-mode-hook
	  (lambda ()
            (setq display-line-numbers-type 'relative)
	    (set-face-attribute 'line-number nil
				:weight 'normal)
	    (set-face-attribute 'line-number-current-line nil
				:foreground (face-attribute 'cursor :background)
				:weight 'bold
                                :background "light sky blue"
				:slant 'normal)))



;; setup the initial scratch file
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; show the column number in the status bar
(setq column-number-mode t)

;; syntax highlighting related
(setq font-lock-maximum-decoration t)
(setq global-font-lock-mode t)

;; match parentheses
(setq show-paren-mode t)
(electric-pair-mode)

;; minibuffer history
(setq savehist-mode t)

;; display the time in status line
(setq display-time-mode t)

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; set the fill column
(setq-default fill-column 80)

;; show trailing whitespace
(setq show-trailing-whitespace 1)
;;; dired stuff
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
	(wnd (current-window-configuration)))
    (if (<= (length files) 2)
	(let ((file1 (car files))
	      (file2 (if (cdr files)
			 (cadr files)
			 (read-file-name
			  "file: "
			  (dired-dwin-target-directory)))))
	  (if (file-newer-than-file-p file1 file2)
	      (ediff-files file2 file1)
	      (ediff-files file1 file1))
	  (add-hook 'ediff-after-quit-hook-internal
		    (lambda ()
		      (setq ediff-after-quit-hook-internal nil)
		      (set-window-configuration wnd ))))
	(error "No more than 2 files should be marked"))))


(defun my-dired-mode-hooks ()
  "set all customisation for dired-mode"
  (setq dired-listing-switches "-al --group-directories-first")
  (setq dired-dwim-target t)
  (define-key dired-mode-map "e" 'ora-ediff-files))

(add-hook 'dired-mode-hook 'my-dired-mode-hooks)

;;; end of dired stuff

;; no tabs see Silicon Valley S03E06
(setq-default indent-tabs-mode nil)

;; untabify all files in a directory
(defun untabyfy-marked-files ()
       (interactive)
       (dolist (file (dired-get-marked-files))
         (find-file file)
         (untabify (point-min) (point-max))
         (save-buffer)
         (kill-buffer nil)))

;; save the window layout at exit
;; (destktop-save-mode 1)

;; =============================================================================
;; frame-undelete-mode enabled
;; =============================================================================
(setq frame-undelete-mode t)

;; =============================================================================
;; make sure that new frames are getting focus
;; =============================================================================
(defun focus-new-client-frame()
  (select-frame-set-input-focus (selected-frame)))

(add-hook 'server-after-make-frame-hook #'focus-new-client-frame)
;; ==========================================================================
;; word and line wrapping settings mostly seen from doom emacs
;; ==========================================================================

;; continue words ast whitespace, instead of in the middle of a word.
(setq-default words-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq sentence-end-double-space nil)
;; required for POSIX compliance
(setq require-final-newline t)

(setq-default auto-fill-mode t)

(add-hook 'text-mode-hook #'visual-line-mode)

;; set up the  load-path
(let ((default-directory  "~/.emacs.d/lisp"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path'(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(add-to-list 'exec-path "/usr/bin/")
(setenv "PATH" (mapconcat 'identity exec-path ":"))


;; =============================================================================
;; window and user interface functions
;; =============================================================================
(defun my-split-window-func ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer))
  )

(global-set-key "\C-x2" 'my-split-window-func)

;; jump backwards to the previous pane
(global-set-key (kbd "S-C-x S-C-o") 'previous-multiframe-window)

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
;; (package-initialize)

(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/elpa/auto-complee*")


;; =============================================================================
;; mic-paren mode for advanced parantheses matching
;; =============================================================================
(require 'mic-paren) ; loading
(paren-activate)     ; activating


;; ;; =============================================================================
;; ;;
;; ;; Begin python-mode related settings
;; ;;
;; ;; =============================================================================

;; (require 'python-mode)
;; (add-to-list 'auto-modalist '("\\.py\\'" . python-mode))
;; (setq python-shell-interpreter '/usr/bin/python)
;; (add-to-list 'load-path "~/.emacs.d/lisp/python-mode")
;; (setq py-install-directory "~/.emacs.d/lisp/python-mode")


;; (use-package pyvenv
;;     :ensure t
;;     :config
;;     (pyvenv-mode t)
;;     ;; set the correct interpreter
;;     (setq pyvenv-post-activate-hooks
;;           (list (lambda ()
;;                   (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
;;     (setq pyvenv-post-deactivate-hooks
;;           (list (lambda ()
;;                   setq python-shell-interpreter "python3"))))


;; ;; ;; enable flycheck
;; ;; (add-hook 'after-init-hook #'global-flycheck-mode)


;; ;; =============================================================================
;; ;; = End python-mode related settings
;; ;; =============================================================================


(use-package python-black
    :demand t
    :after Python
    :hook (python-mode . python-black-on-save-mode-enable-dwim))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs.d/elpa/auto-complete*/dict")
(add-hook 'js2-mode-hook 'ac-js2-mode)


(require 'auto-complete)
(global-auto-complete-mode t)

(use-package auto-complete
    :config (ac-flyspell-workaround))

;; =============================================================================
;; hide-show mode
;; =============================================================================
(require 'hideshow)

(defvar-local hs-current-level 1
  "currently hidden level of hideshow.")

(defvar-local hs-prev-levek-active nil
  "Deactivate `hs-hide-prev-level' when `hs-current-level' is 1.")

(defvar-local hs-next-level-active t
"Deactivate `hs-hide-next-level' when there are  no more hs overlays.")

(defun hs-overlays-p (&optional ignore-comments)
  "Return non-nil if there  are still hs-overlays.
Ignore comments if IGNORE-COMMENTS is non-nil"
  (let (ol)
    (save-excursion
      (goto-char (point-min))
      (while (and (null (eobp))
                  (null (and (setq ol (hs-overlay-at (goto-char (next-overlay-change (point)))))
                             (or (null ignore-comments)
                                 (null (eq overlay-get ol 'hs) 'comment))))))
      (null (eobp)))))

(defun hs-hide-prev-level ()
  "Hide level `hs-current-level' -1."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq hs-current-level (max(1- hs-current-level) 1))
    (hs-hide-level hs-current-level)))

(defun hs-hide-next-level ()
  "Hide level `hs-current-level' +1."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq hs-current-level (1+ hs-current-level ))
    (hs-hide-level hs-current-level)))

(define-key hs-minor-mode-map (kbd "C-c @ >") #'hs-hide-next-level)
(define-key hs-minor-mode-map (kbd "C-c @ <") #'hs-hide-prev-level)

(easy-menu-add-item hs-minor-mode-menu nil ["Hide previous level" hs-hide-prev-level (> hs-current-level 1)] "Toggle Hiding")
(easy-menu-add-item hs-minor-mode-menu nil ["Hide next level" hs-hide-next-level (hs-overlays-p t)] "Toggle Hiding")

;; =============================================================================
;; end hideshow stuff
;; =============================================================================

;; =============================================================================
;; xml mode stuff
;; =============================================================================
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|<\[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional keybinding, easuier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; =============================================================================
;; = End xml mode stuff
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
  (insert (format-time-string "%H:%M:%S" )))

(defun insert-current-date-time()
  "insert the current date and time at point."
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))


(defun epoch-to-datetime ()
  "Convert the selection to datetime"
  (interactive)
  (let* ((epoch (string-to-number (buffer-substring (mark) (point))))
         (datetime (format-time-string "%Y/%m/%d %H:%M:%S" (seconds-to-time epoch))))
    (delete-region (mark) (point))
    (insert datetime)))


(global-set-key "\C-cd" 'insert-current-date-time)
(global-set-key "\C-ct" 'insert-current-time)
(global-set-key "\C-cu" 'epoch-to-datetime)


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

;;  enable auto-fill-mode in org-mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; enable flyspell-mode in org-mode
(add-hook 'org-mode-hook 'flyspell-mode t)

;; enable auto complete mode in org mode
(add-hook 'org-mode-hook 'auto-complete-mode t)

(org-babel-do-load-languages
 'org-babel-load-languages
   '(
      (plantuml . t)
     (python . t)
     (ditaa . t)
     )
   )

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; integrate plantuml with org-mode
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(setq plantuml-default-exec-mode 'jar)
(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-jar-path "/usr/share/java/plantuml.jar")
(setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")

;;
;; allow exporting to spreadsheets
;;
(defun abc/org-table-to-tsv ()
  (interactive)
  (let ((temp-buffer-name "*dump-buffer*"))
    (org-mark-element)
    (kill-ring-save nil nil 't)
    (switch-to-buffer temp-buffer-name)
    (yank)
    (beginning-of-buffer)
    (delete-char 2)
    ;; remove the leading pipe char
    (perform-replace "
| " "
" nil nil nil nil nil  (point-min) (point-max))
    ;; remove trailing pipe char
    (perform-replace " |
" "
" nil nil nil nil nil  (point-min) (point-max))
    ;; remove internal pipe chars
    (replace-regexp " +| " "	"
                    nil (point-min) (point-max))
    ;; remove terminal pipe char
    (replace-regexp " +|
" "
"
nil (point-min) (point-max))
    ;; remove separators
    (replace-regexp "^|-+.*
" ""
nil (point-min) (point-max))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer temp-buffer-name)))


;; -----------------------------------------------------------------------------
;; End of org-mode config
;; -----------------------------------------------------------------------------

(defun org-babel-execute:yaml (body params) body)

;; enable auto-fill-mode in latex mode
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; enable auto-fill-mode in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;; ansible-lint
;; compile regex for ansible-lint
;; (require 'compile)
;; (add-to-list 'compilation-error-regexp-alist
;;              'yam)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(yaml "^\\(.*?\\)):\\([0-9]+\\)" 1 2)
;;              )

;; replace make -k with ansible-lint, with an UTF-8 locale to avoid craches
(defun ansible-lint-errors()
  (make-local-variable 'compile-command)
  (let ((ansible-lint-command "ansible-lint ")(loc "LANG=C.UTF-8 "))
    (setq compile-command (concat loc ansible-lint-command buffer-file-name)))
  )

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(defun my-yaml-mode-hooks ()
  "Add all the customisations for yaml-mode here"
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (set-fill-column 9999)
  (auto-complete-mode t)
  (setq ac-yaml 'ac-source-yaml)
  (ansible 1)
  (ansible-lint-errors)
  (lsp-deferred)
  )


(add-hook 'yaml-mode-hook
          'my-yaml-mode-hooks)

;; (add-hook 'yaml-mode-hook 'ansible-lint-errors)

(global-set-key (kbd "<f5>") 'compile)

;; =============================================================================
;; Additional set-up for encrypted yaml files ( ansible-minor-mode)
;; =============================================================================
;; (setq ansible-vault--vault-id "erwin.baeyens@benerail.com")
;; (setq ansible-vault--password-file "~/.config/.vault_pass.txt")
(setq ansible-vault-password-file "~/.config/.vault_pass.txt")
(add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)

(global-set-key (kbd "C-c b") 'ansible-decrypt-buffer)
(global-set-key (kbd "C-c g") 'ansible-encrypt-buffer)

;; -----------------------------------------------------------------------------
;; k8s-mode setup
;; -----------------------------------------------------------------------------
(require ' k8s-mode)
(use-package k8s-mode
    :ensure t
    :hook (k8s-mode . yas-minor-mode))

;; set indent offset
(setq k8s-indent-offset 2)
;; The site docs URL
(setq k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/")
;; The defautl API version
(setq k8s-site-docs-version "v1.3")
;; The browser funtion to browse the docs site. Default is `browse-url-browser-function`
(setq k8s-search-documentation-browser-function nil)
; Should be a X11 browser
;(setq k8s-search-documentation-browser-function (quote browse-url-firefox))



;; =============================================================================
;; Macro's
;; =============================================================================

(defalias 'capitalize-name
   (kmacro "C-s - SPC n a m e : <return> C-f M-c C-n"))

(global-set-key (kbd "<f6>") 'capitalize-name)

(defalias 'ansible-builtin
   (kmacro "a n s i b l e . b u i l t i n . C-e C-n"))
(global-set-key (kbd "<f7>") 'ansible-builtin)


;; =============================================================================
;; Multiple cursor setup
;; =============================================================================
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; =============================================================================
;; various functions
;; =============================================================================
(defun write-out-region ()
  "function to writeout the current region to a file"
  (interactive)
  (write-region (region-beginning) (region-end)
                (read-file-name "Save region to: " nil nil nil))
  )



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
;; (add-hook 'web-mode-hook 'hs-minor-mode)


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
(add-hook 'c-mode-hook #'lsp)


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
;; add a checkmark on a table line
;; =============================================================================
(defun check-cell()
  (interactive)
  (let ((cell(org-table-get-field)))
    (if (string-match "[[:graph:]]" cell)
        (org-table-bank-field)
        (insert "X"))
    (org-table-next-row))
  )

(global-set-key (kbd "s-f") 'check-cell)


;; ===========================================================================
;; markdown mode setup
;; ===========================================================================
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavoured Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


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

;; tell emacs where to find the mysql binary
(setq sql-mysql-program "/usr/bin/mysql")

;;
;; copy and paste to and from other applications but emacs

;; (xclip-mode 1)
(setq
     x-select-enable-clipboard t
     x-select-enable-primary t
     x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
     x-stretch-cursor t)


;; =============================================================================
;; vc-msg mode hook
;; =============================================================================
(defun vc-msg-hook-setup (vcs-typ commit-info)
  ;; copy commit info id to clipboard
  (message (froat " %s\n%s\n%s\n%s"
                  (plist-get commit-info :id)
                  (plist-get commit-info :author)
                  (plist-get commit-info :author-time)
                  (plist-get commit-info :author-summary))))
(add-hook 'vc-msg-hook 'vc-msg-hook-setup)

;; ________________________________________________________________________________
;; format xml the easy way
;;
;; --------------------------------------------------------------------------------
(defun format-xml()
  (interactive)
  (shell-command-on-region 1 (point-max) "xmllint --format -" (current-buffer) t))

;; -----------------------------------------------------------------------------
;;  format json the easy way
;;
;; -----------------------------------------------------------------------------
(defun format-json()
  (interactive)
  (shell-command-on-region 1 (point-max) "jsonlint --format -" (current-buffer) t))

;; =============================================================================
;; set the default directory to use so that vterm works
;; =============================================================================
(use-package vterm
    :load-path "~/.emacs.d/elpa/vterm-20240102.1640")

(setq default-directory "~/")
(define-key vterm-mode-map (kbd "C-q") 'vterm-send-next-key)

(setq default-directory "~/")
(define-key vterm-mode-map (kbd "C-q") 'vterm-send-next-key)

(use-package math-preview
    :custom (math-preview-command "/usr/local/bin/math-preview"))

;; ===============================================================
;; automagically recreate the *scratch* buffer
;; ===============================================================
(defun prepare-scratch-for-kill ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (add-hook 'kill-buffer-query-functions
              'kill-scratch-buffer t)))

(defun kill-scratch-buffer ()
  (let (kill-buffer-query-functions)
    (kill-buffer (current-buffer)))
  (prepare-scratch-for-kill)
  nil)

;; =============================================================================
;; automagically generated part. Best if not touched
;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add undelete-frame-mode)
 '(column-number-mode t)
 '(custom-safe-themes
   '("6876d0eb1bcef5ce6a71434d7c19a66a1a14378e22dce3b72b1fbc51aeb5e18d" default))
 '(display-line-numbers-type 'relative)
 '(display-time-mode t)
 '(eudc-default-return-attributes 'all)
 '(eudc-server-hotlist '(("" . bbdb) ("ldap://pro-ipa-blo.psop.be" . ldap)))
 '(eudc-use-raw-directory-names t)
 '(font-use-system-font t)
 '(global-display-line-numbers-mode t)
 '(k8s-indent-offset 2)
 '(markdown-command "/usr/bin/pandoc")
 '(nxml-slash-auto-complete-flag t)
 '(org-export-with-sub-superscripts '{})
 '(org-safe-remote-resources
   '("\\`https://www\\.cybertec-postgresql\\.com\\(?:/\\|\\'\\)"))
 '(package-selected-packages
   '(lsp-docker lsp-jedi flycheck-yamllint math-preview ansible esup plantuml-mode wgrep dired-subtree k8s-mode vc-msg terraform-mode crontab-mode lsp-ui lsp-mode python-black cl-libify pyenv pyvenv vterm-toggle orgtbl-aggregate auto-complete-nxml html5-schema ## package-selected-packages
                '(yasnippet yaml-mode web-mode typing time-ext tile swiper-helm swiper ssh-tunnels ssh-deploy ssh-config-mode ssh-agency ssh speed-type python-mode python powershell org-gnome org nlinum multiple-cursors mic-paren magit load-dir json-mode js2-mode jinja2-mode icicles gandalf-theme flymake-yaml flymake-python-pyflakes flymake-php flymake-json flycheck-pyflakes flycheck-pycheckers flycheck-phpstan flycheck datetime-format datetime counsel bison-mode auto-complete-exuberant-ctags auto-complet ac-rtags ac-php-core ac-php ac-html-csswatcher ac-html-angular ac-html ac-etags magit-popup with-editor treepy graphql git-commit ghub dash async highlight-indent-guides vala-snippets vala-mode flymake-vala yafolding keychain-environment ox-asciidoc adoc-mode dired-subtree markdown-mode markdownfmt markdown-toc markdown-preview-mode markdown-preview-eww markdown-mode+ projectile ace-window pyenv-mode ascii-art-to-unicode md-readme org-link-minor-mode helm-swoop ag wgrep-ag shackle autotetris-mode vterm aggressive-indent aggressive-fill-paragraph lua-mode flymake-lua ivy folding fold-this fold-dwim-org fold-dwim hideshow-org dash-functional dash-at-point latex-unicode-math-mode latex-preview-pane latex-pretty-symbols lorem-ipsum latex-math-preview cdlatex cobol-mode)))
 '(plantuml-default-exec-mode 'jar t)
 '(plantuml-executable-path "/usr/bin/plantuml" t)
 '(plantuml-jar-path "/usr/share/java/plantuml.jar" t)
 '(reb-re-syntax 'string)
 '(safe-local-variable-values '((conding . utf-8)))
 '(send-mail-function 'mailclient-send-it)
 '(size-indication-mode t)
 '(sql-electric-stuff 'semicolon)
 '(sql-mysql-login-params '(user password server database port))
 '(sql-mysql-program "mysql")
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))

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
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight regular :height 128 :width normal)))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

;; (set-face-attribute 'default-nil
;;                     :family "JetBrains Mono"
;;                     :foundry "outline"
;;                     :slant 'normal'
;;                     :weight 'normal'
;;                     :height 120
;;                     :width 'semi-condensed')

