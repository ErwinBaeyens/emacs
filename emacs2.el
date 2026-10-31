;; -*- mode: emacs-lisp; fill-column: 75; comment-column: 50; lexical-binding: t; -*-
;; .emacs.el

;;; Core UI --------------------------------------------------------------------

(require 'color)

(set-cursor-color "Cornflower blue")
(setq default-frame-alist '((cursor-color . "Cornflower blue"))
      inhibit-splash-screen t
      inhibit-startup-message t
      frame-title-format "emacs - %b - %f"
      search-highlight t
      user-full-name "Erwin Baeyens"
      user-mail-address "Erwin.Baeyens@benerail.com")

(tool-bar-mode 0)
(load-theme 'wombat t)

(when (display-graphic-p)
  ;; On X, integrate clipboard and put scroll bar on the right.
  (setq x-select-enable-clipboard t)
  (set-scroll-bar-mode 'right))

;;; Editing defaults ------------------------------------------------------------

(setq default-directory "~/"
      dired-listing-switches "-al --group-directories-first"
      show-trailing-whitespace t
      sentence-end-double-space nil
      require-final-newline t
      display-line-numbers-type 'relative
      size-indication-mode t
      column-number-mode t
      savehist-mode t
      display-time-mode t
      show-paren-mode t)

(setq-default fill-column 80
              indent-tabs-mode nil
              truncate-lines t
              truncate-partial-width-windows nil
              auto-fill-function 'do-auto-fill
              word-wrap t)

(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)

(setq initial-major-mode 'org-mode
      initial-scratch-message nil)

(if (version< emacs-version "26.1")
    (global-linum-mode 1)
  (global-display-line-numbers-mode 1))

(add-hook 'display-line-numbers-mode-hook
          (lambda ()
            (set-face-attribute 'line-number nil :weight 'normal)
            (set-face-attribute 'line-number-current-line nil
                                :foreground (face-attribute 'cursor :background)
                                :weight 'bold
                                :background "light sky blue"
                                :slant 'normal)))

(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)

;;; Paths ----------------------------------------------------------------------

(let ((default-directory "~/.emacs.d/lisp"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(add-to-list 'exec-path "/usr/bin/")
(setenv "PATH" (mapconcat #'identity exec-path ":"))

;;; Dired ----------------------------------------------------------------------

(defun my/ediff-files ()
  "Ediff two marked files in Dired, older on left/newer on right."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let* ((file1 (car files))
               (file2 (if (cdr files)
                          (cadr files)
                        (read-file-name "File: " (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))

(defun my/dired-mode-hook ()
  "Custom settings for `dired-mode'."
  (setq dired-dwim-target t)
  (define-key dired-mode-map (kbd "e") #'my/ediff-files))

(add-hook 'dired-mode-hook #'my/dired-mode-hook)

(defun my/untabify-marked-files ()
  "Untabify all marked files in Dired."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (untabify (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;;; Window helpers --------------------------------------------------------------

(defun my/split-window-and-follow ()
  "Split window vertically and show `other-buffer' in the new pane."
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun my/swap-windows ()
  "Swap buffers when exactly 2 windows are visible."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this (selected-window))
             (other (next-window))
             (this-buffer (window-buffer this))
             (other-buffer (window-buffer other)))
        (set-window-buffer this other-buffer)
        (set-window-buffer other this-buffer))
    (message "Need exactly 2 windows to swap.")))

(global-set-key (kbd "C-x 2") #'my/split-window-and-follow)
(global-set-key [C-tab] #'my/swap-windows)

(defun my/focus-new-client-frame ()
  "Focus newly created client frame."
  (select-frame-set-input-focus (selected-frame)))

(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;;; Packages -------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Folding / hideshow ---------------------------------------------------------

(require 'hideshow)

(defvar-local hs-current-level 1
  "Currently hidden level of hideshow.")

(defun my/hs-hide-prev-level ()
  "Hide previous hideshow level."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq hs-current-level (max (1- hs-current-level) 1))
    (hs-hide-level hs-current-level)))

(defun my/hs-hide-next-level ()
  "Hide next hideshow level."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq hs-current-level (1+ hs-current-level))
    (hs-hide-level hs-current-level)))

(define-key hs-minor-mode-map (kbd "C-c @ >") #'my/hs-hide-next-level)
(define-key hs-minor-mode-map (kbd "C-c @ <") #'my/hs-hide-prev-level)

;;; XML ------------------------------------------------------------------------

(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|<[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook #'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") #'hs-toggle-hiding)

;;; Root file helper -----------------------------------------------------------

(defvar find-file-root-prefix "/sudo:root@localhost:"
  "Filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files opened via `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook run after opening a file via `find-file-root'.")

(defun find-file-root ()
  "Open a file as root via TRAMP."
  (interactive)
  (require 'tramp)
  (let* ((file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)
    (when tramp
      (setq path (tramp-file-name-localname tramp)
            dir (file-name-directory path)))
    (when (setq file (read-file-name "Find file (UID=0): " dir path))
      (find-file (concat find-file-root-prefix file))
      (setq find-file-root-history file-name-history)
      (run-hooks 'find-file-root-hook))))

(global-set-key (kbd "C-x C-t") #'find-file-root)

;;; Spelling -------------------------------------------------------------------

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell"
        ispell-extra-args '("-d" "en_GB"))))

;;; Run current file -----------------------------------------------------------

(defun run-current-file ()
  "Execute current file based on extension."
  (interactive)
  (let* ((suffix-map '(("pl" . "perl")
                       ("py" . "python")
                       ("py3" . "python3")
                       ("rb" . "ruby")
                       ("go" . "go run")
                       ("js" . "node")
                       ("sh" . "bash")
                       ("vbs" . "cscript")
                       ("tex" . "pdflatex")
                       ("latex" . "pdflatex")
                       ("java" . "javac")))
         (fname (buffer-file-name))
         (ext (and fname (file-name-extension fname)))
         (prog (and ext (cdr (assoc ext suffix-map))))
         (cmd (and prog fname (format "%s %s" prog (shell-quote-argument fname)))))
    (unless fname
      (save-buffer)
      (setq fname (buffer-file-name)
            ext (and fname (file-name-extension fname))
            prog (and ext (cdr (assoc ext suffix-map)))
            cmd (and prog fname (format "%s %s" prog (shell-quote-argument fname)))))
    (when (buffer-modified-p)
      (save-buffer))
    (cond
     ((string-equal ext "el") (load-file fname))
     ((string-equal ext "java")
      (shell-command cmd "*run-current-file output*")
      (shell-command
       (format "java %s" (file-name-sans-extension (file-name-nondirectory fname)))))
     (prog
      (message "Running...")
      (shell-command cmd "*run-current-file output*"))
     (t (message "No recognized program file suffix for this file.")))))

(global-set-key (kbd "<f8>") #'run-current-file)

;;; Time/date helpers ----------------------------------------------------------

(defun insert-current-time ()
  "Insert current time at point."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-current-date-time ()
  "Insert ISO-like date-time including offset at point."
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    (let ((x (format-time-string "%z")))
      (concat (substring x 0 3) ":" (substring x 3 5))))))

(defun epoch-to-datetime ()
  "Convert selected epoch seconds into formatted datetime."
  (interactive)
  (let* ((epoch (string-to-number (buffer-substring (mark) (point))))
         (datetime (format-time-string "%Y/%m/%d %H:%M:%S" (seconds-to-time epoch))))
    (delete-region (mark) (point))
    (insert datetime)))

(global-set-key (kbd "C-c d") #'insert-current-date-time)
(global-set-key (kbd "C-c t") #'insert-current-time)
(global-set-key (kbd "C-c u") #'epoch-to-datetime)

;;; Org ------------------------------------------------------------------------

(require 'org)
(require 'ox-asciidoc)
(require 'ox-md)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;; Keep your original intent but fix typo:
(global-set-key (kbd "C-c b") #'org-switchb)

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))
      org-capture-templates
      '(("t" "todo" entry (file+headline "~/Dropbox/Erwin Stuff/todo/tasks.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))

(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'auto-complete-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ditaa . t)))

(defun org-babel-execute:yaml (body _params)
  body)

;;; LSP / Copilot --------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
        lsp-client-packages nil)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev "newest"
            :branch "main")
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)))

(setq copilot-chat-use-agent-mode t)

;;; Python ---------------------------------------------------------------------

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq python-shell-interpreter "/usr/bin/python"
      py-install-directory "~/.emacs.d/lisp/python-mode")
(add-to-list 'load-path "~/.emacs.d/lisp/python-mode")

(use-package python
  :hook (python-mode . hs-minor-mode))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env "/bin/python3"))))
        pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(require 'auto-complete-config)
(require 'auto-complete)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete*/dict")
(add-hook 'js2-mode-hook #'ac-js2-mode)
(global-auto-complete-mode 1)

(use-package auto-complete
  :config
  (ac-flyspell-workaround))

(defun my/python-mode-hook ()
  "Python mode customizations."
  (set-fill-column 132)
  (auto-complete-mode 1)
  (lsp-deferred)
  (hs-minor-mode 1))

(add-hook 'python-mode-hook #'my/python-mode-hook)

;;; YAML / Ansible -------------------------------------------------------------

(require 'compile)
(require 'yaml-mode)

(add-to-list 'compilation-error-regexp-alist 'yaml)
(add-to-list 'compilation-error-regexp-alist-alist
             '(yaml "^\\(.*?\\):\\([0-9]+\\)" 1 2))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(defvar clean-up-file
  (expand-file-name "lisp/ansible_cleanup.el" user-emacs-directory)
  "Optional local clean-up file for YAML buffers.")

(defun my/ansible-lint-errors ()
  "Prepare `compile-command' for ansible-lint in current YAML file."
  (interactive)
  (when buffer-file-name
    (let ((ansible-lint-args '("LANG=C.UTF-8" "ansible-lint" "--offline")))
      (setq-local compile-command
                  (concat
                   (mapconcat #'identity ansible-lint-args " ")
                   " "
                   (shell-quote-argument buffer-file-name))))))

(defun my/yaml-mode-hook ()
  "Customizations for `yaml-mode'."
  (define-key yaml-mode-map (kbd "C-m") #'newline-and-indent)
  (set-fill-column 9999)
  (auto-complete-mode 1)
  (setq ac-yaml 'ac-source-yaml
        font-lock-mode t)
  (ansible-mode 1)
  (lsp-deferred)
  (display-line-numbers-mode 1)
  (my/ansible-lint-errors)
  (copilot-mode 1)
  (when (file-exists-p clean-up-file)
    (load clean-up-file)))

(add-hook 'yaml-mode-hook #'my/yaml-mode-hook)

(setq ansible-vault-password-file "~/.config/.vault_pass.txt")
(add-hook 'ansible-hook #'ansible-auto-decrypt-encrypt)
(global-set-key (kbd "C-c b") #'ansible-decrypt-buffer)
(global-set-key (kbd "C-c g") #'ansible-encrypt-buffer)

;;; k8s ------------------------------------------------------------------------

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

(setq k8s-indent-offset 2
      k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/"
      k8s-site-docs-version "v1.3"
      k8s-search-documentation-browser-function nil)

;;; Misc utilities -------------------------------------------------------------

(defalias 'capitalize-name
  (kmacro "C-s - SPC n a m e : <return> C-f M-c C-n"))
(global-set-key (kbd "<f6>") #'capitalize-name)

(defalias 'ansible-builtin
  (kmacro "a n s i b l e . b u i l t i n . C-e C-n"))
(global-set-key (kbd "<f7>") #'ansible-builtin)

(defun write-out-region ()
  "Write current region to a file."
  (interactive)
  (write-region (region-beginning)
                (region-end)
                (read-file-name "Save region to: " nil nil nil)))

(global-set-key (kbd "C-x g") #'magit-status)

(global-set-key (kbd "C-'") #'toggle-hiding)
(global-set-key (kbd "C-=") #'toggle-selective-display)

(add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
(add-hook 'sh-mode-hook #'hs-minor-mode)

(require 'wgrep)
(setq wgrep-auto-save-buffer t
      wgrep-enable-key "r")

;;; C mode ---------------------------------------------------------------------

(defun c-lineup-arglist-tabs-only (_ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees") filename))
                (setq indent-tabs-mode t
                      show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

(add-hook 'c-mode-hook #'lsp)

;;; Ibuffer --------------------------------------------------------------------

(global-set-key (kbd "C-x C-b") #'ibuffer)
(setq ibuffer-show-empty-filter-groups nil
      ibuffer-expert t)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;;; Markdown -------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown." t)
(autoload 'gfm-mode "markdown-mode" "Major mode for GitHub Flavoured Markdown." t)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; Export helpers -------------------------------------------------------------

(defun run-a2x-to-generate-pdf ()
  "Generate PDF for current AsciiDoc file."
  (when (eq major-mode 'adoc-mode)
    (shell-command-to-string (format "a2x -f pdf %s" buffer-file-name))))

(add-hook 'after-save-hook #'run-a2x-to-generate-pdf)

(defun format-xml ()
  "Pretty-print XML in current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "xmllint --format -" (current-buffer) t))

(defun format-json ()
  "Pretty-print JSON in current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "jsonlint --format -" (current-buffer) t))

;;; Clipboard / SQL / vterm ----------------------------------------------------

(setq sql-mysql-program "/usr/bin/mysql"
      x-select-enable-clipboard t
      x-select-enable-primary t
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      x-stretch-cursor t)

(use-package vterm
  :load-path "~/.emacs.d/elpa/vterm-20240102.1640")

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key))

;;; vc-msg ---------------------------------------------------------------------

(defun vc-msg-hook-setup (_vcs-type commit-info)
  (message "%s\n%s\n%s\n%s"
           (plist-get commit-info :id)
           (plist-get commit-info :author)
           (plist-get commit-info :author-time)
           (plist-get commit-info :author-summary)))

(add-hook 'vc-msg-hook #'vc-msg-hook-setup)

;;; Scratch buffer persistence -------------------------------------------------

(defun prepare-scratch-for-kill ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (add-hook 'kill-buffer-query-functions #'kill-scratch-buffer t)))

(defun kill-scratch-buffer ()
  (let (kill-buffer-query-functions)
    (kill-buffer (current-buffer)))
  (prepare-scratch-for-kill)
  nil)

;;; Custom block ---------------------------------------------------------------

(custom-set-variables
 ;; Keep Custom-managed settings in one place.
 '(column-number-mode t)
 '(custom-safe-themes
   '("6876d0eb1bcef5ce6a71434d7c19a66a1a14378e22dce3b72b1fbc51aeb5e18d" default))
 '(dired-listing-switches "-al --group-directories-first")
 '(display-line-numbers-type 'relative)
 '(display-time-mode t)
 '(eudc-default-return-attributes 'all)
 '(eudc-server-hotlist '(("" . bbdb) ("ldap://pro-ipa-blo.psop.be" . ldap)))
 '(eudc-use-raw-directory-names t)
 '(global-display-line-numbers-mode t)
 '(global-whitespace-mode nil)
 '(k8s-indent-offset 2)
 '(lsp-trim-trailing-whitespace nil)
 '(lsp-warn-no-matched-clients nil)
 '(markdown-command "/usr/bin/pandoc")
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files nil)
 '(org-export-with-sub-superscripts '{})
 '(org-safe-remote-resources
   '("\\`https://www\\.cybertec-postgresql\\.com\\(?:/\\|\\'\\)"))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(persistent-scratch-autosave-mode t)
 '(persistent-scratch-backup-directory "~/.emacs.d/scratch")
 '(persistent-scratch-save-file "/home/exl217/.emacs.d/scratch/.persistent-scratch")
 '(reb-re-syntax 'string)
 '(safe-local-variable-values '((conding . utf-8)))
 '(send-mail-function 'mailclient-send-it)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(sql-electric-stuff 'semicolon)
 '(sql-mysql-login-params '(user password server database port))
 '(sql-mysql-program "mysql" t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))

(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column
      highlight-indent-guides-responsive 'stack)

(custom-set-faces
 '(default ((t (:family "Cousine" :foundry "GOOG" :slant normal :weight regular :height 119 :width normal)))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
