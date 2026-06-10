;; -*- lexical-binding: t; -*-

;;;; Bootstrapping Elpaca
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks (windows):
;; (elpaca-no-symlink-mode)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq use-package-always-ensure t)

(use-package compat)

;;;; Completion & Menu Packages

(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :custom
  (vertico-count 15)
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init (savehist-mode))

;; Remembers cursor position in closed files
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;(completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :init
  (marginalia-mode))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; https://github.com/oantolin/embark/
(use-package embark
  :bind
  (("s-;" . embark-act))         ;; pick some comfortable binding
   ;;("s-m" . embark-dwim)        ;; good alternative: M-.
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; https://www.matem.unam.mx/~omar/apropos-emacs.html#the-case-against-whics-key-a-polemic
;; TODO: worth reading over! https://github.com/minad/consult
(use-package consult
  :hook 
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("s-<down>" "s-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point))

;; TODO: set up Corfu later
;; https://github.com/meatcar/emacs.d
;; https://github.com/minad/corfu
(use-package corfu
  :bind
  ("A-SPC" . completion-at-point)
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

;; https://kristofferbalintona.me/posts/202203130102/
;; https://www.youtube.com/watch?v=Vx0bSKF4y78&t=768s
(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  :config
  (advice-add 'dabbrev-capf :around #'cape-wrap-silent)
  (advice-add 'dabbrev-capf :around #'cape-wrap-purify))

;; TODO integrate Emacs into Window Buffer

;;;; Development

;; Setup: https://github.com/karthink/gptel
(use-package gptel :defer t)

;; Essential to have a REPL MCP for long time horizon LISP dev:
;; https://github.com/bhauman/clojure-mcp

(use-package agent-shell
  :config
  (setq agent-shell-openai-authentication
	(agent-shell-openai-make-authentication :login t))
  (setq agent-shell-preferred-agent-config (agent-shell-openai-make-codex-config)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;; ELDOC
;; Eldoc provides helpful inline documentation for functions and variables
;; in the minibuffer, enhancing the development experience. It can be particularly useful
;; in programming modes, as it helps you understand the context of functions as you type.
;; This package is built-in, so there's no need to fetch it separately.
;; The following line enables Eldoc globally for all buffers.
(use-package eldoc
  :ensure nil                                ;; This is built-in, no need to fetch it.
  :config
  (setq eldoc-idle-delay 0.3)                  ;; Automatically fetch doc help
  (setq eldoc-echo-area-use-multiline-p nil) ;; We use the "K" floating help instead
                                             ;; set to t if you want docs on the echo area
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

;; TODO: consider using eldoc-box later
;(use-package eldoc-box
;  :bind
;  ("s-h" . eldoc-box-help-at-point))

;; LSP
(use-package eglot
  :ensure nil
  :defer t)

(use-package flymake
  :ensure nil)

;; Terminal
(use-package eat
  :defer t)

;; Clojure

(use-package clojure-mode :defer t)

(use-package cider :defer t
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . cider-mode)
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-save-file-on-load t)
  (cider-use-overlays t)
  (cider-overlays-use-font-lock t))


(use-package clj-refactor :defer t
  :after cider
  :hook (clojure-mode . clj-refactor-mode))

;;(use-package add-node-modules-path)

;; Version Control for Jujutsu
;;;; Version Control using Jujutsu

(use-package magit
  :commands (magit-status magit-dispatch))

(use-package transient)

(use-package with-editor)

(use-package vc-jj
  :after project)

;; https://blog.alarsyo.net/posts/2025/02/on-jujutsu-and-magit/#know-more-about-jj
(use-package majutsu
  :ensure (:host github :repo "0WD0/majutsu")
  :after (magit transient with-editor)
  :commands (majutsu majutsu-log)
  :config
  ;; Keep Majutsu buffers readable with your Meow setup.
  ;; Majutsu already supplies its own modal-style keys inside its buffers:
  ;; n/p navigate, RET visits, ? opens the dispatcher.
  )

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode))
  :custom
  (markdown-header-scaling t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-max-image-size '(600 . 400))
  (markdown-hide-markup nil)
  (markdown-command "pandoc"))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 90)
  (visual-fill-column-center-text t))

;; Avy
(defun my/avy-goto-delimiter ()
  (interactive)
  (avy-jump "[][(){}\"]"))

(use-package avy
  :init
  (avy-setup-default)
  :bind
  (("s-a" . 'avy-goto-char-timer)
   ("s-s" . my/avy-goto-delimiter))
  :custom
  (avy-timeout-seconds 0.8))

;; Helpful
(use-package helpful)

;; TODO: Wgrep
;; Can allow editing grep results in collected buffer
(use-package wgrep
  :defer t)

;;;; Keybinds

;; http://xahlee.info/emacs/emacs/emacs_keybinding_list.html
(defun remove-alt-default-bindings ()
  (let ((alt-keys
         '("A-SPC" "A-!" "A-$" "A-+" "A--" "A-<" "A->" "A-?" "A-C" "A-E"
           "A-L" "A-P" "A-R" "A-S" "A-T" "A-Y" "A-[" "A-]" "A-c" "A-m"
           "A-o" "A-u" "A-x" "A-{" "A-|" "A-}")))
    (dolist (key-str alt-keys)
      (let ((key (condition-case nil (kbd key-str) (error nil))))
        (when key
          (when (commandp (lookup-key (current-global-map) key))
            (global-unset-key key))
          (when (boundp 'key-translation-map)
            (define-key key-translation-map key nil))
          (when (boundp 'function-key-map)
            (define-key function-key-map key nil))
          (when (boundp 'local-function-key-map)
            (define-key local-function-key-map key nil))
          (when (boundp 'input-decode-map)
            (define-key input-decode-map key nil)))))
    (message "Remove Default Alt Bindings: %d" (length alt-keys))))

;; macOS input
(when (eq system-type 'darwin)
  (setq ns-right-option-modifier 'super
        ns-right-control-modifier 'alt)
  (remove-alt-default-bindings))

;; Great Smartparens guide: 
;; https://ebzzry.com/en/emacs-pairs/
(defun my/sp-wrap-double-quote ()
  (interactive)
  (sp-wrap-with-pair "\""))

(use-package smartparens
  :hook 
  ((prog-mode . smartparens-mode)
   (text-mode . smartparens-mode)
   (markdown-mode . smartparens-mode))
  ;;(prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :bind
  (;; Traversal
   ("A-p" . sp-forward-sexp)
   ("A-;" . sp-backward-sexp)
   ("A-o" . sp-down-sexp)
   ("A-l" . sp-backward-down-sexp)
   ("A-i" . sp-up-sexp)
   ("A-k" . sp-backward-up-sexp)
   ;; Manipulation
   ;; Consider Wrapping Fn: https://ebzzry.com/en/emacs-pairs/
   ("A-r" . sp-splice-sexp)
   ("A-e" . sp-unwrap-sexp)
   ("A-w" . sp-rewrap-sexp)
   ("A-f" . sp-forward-slurp-sexp)
   ("A-d" . sp-backward-slurp-sexp)
   ("A-s" . sp-forward-barf-sexp)
   ("A-a" . sp-backward-barf-sexp))
  :config
  ;; load default config
  (require 'smartparens-config)

  ;; Wrapping Menu
  (defvar my/sp-wrap-menu-items
    '((?s "square" sp-wrap-square)
      (?c "curly"  sp-wrap-curly)
      (?r "round"  sp-wrap-round)
      (?g "string"  my/sp-wrap-double-quote))
    "Single-key Smartparens wrapping menu entries.")

  (defun my/sp-wrap-menu--render ()
    "Render a Meow-like key table for `my/sp-wrap-menu'."
    (let* ((cell-width 25)
           (columns (max 1 (min 5 (/ (frame-width) cell-width)))))
      (string-trim-right
       (mapconcat
	#'identity
	(cl-loop
	 for (key label _) in my/sp-wrap-menu-items
	 for index from 0
	 collect
	 (concat
          (propertize
           (format "%-8s" (char-to-string key))
           'face 'font-lock-constant-face)
          (propertize " → " 'face 'font-lock-comment-face)
          (propertize
           (format "%-13s" label)
           'face 'font-lock-function-name-face)
          (if (= (1- columns) (mod index columns)) "\n" " ")))
	""))))

  (defun my/sp-wrap-menu--read ()
    "Read one wrapping key from `my/sp-wrap-menu-items'."
    (read-char-choice
     (concat (my/sp-wrap-menu--render) "\nWrap with: ")
     (mapcar #'car my/sp-wrap-menu-items)))

  (defun my/sp-wrap-menu (&optional arg)
    "Meow-like popup menu for Smartparens wrapping.
     With an active region, wrap the region.  Otherwise, defer to the
     selected Smartparens wrapping command."
    (interactive "P")
    (let* ((key (my/sp-wrap-menu--read))
           (command (nth 2 (assq key my/sp-wrap-menu-items))))
      (unless (commandp command)
	(user-error "No Smartparens wrapper for %c" key))
      (let ((current-prefix-arg arg))
	(call-interactively command)))))

;; Unused Binds
; ("A-l" . sp-beginning-of-sexp)
; ("A-;" . sp-end-of-sexp)
; ("A-m" . sp-next-sexp)
; ("A-n" . sp-previous-sexp)
; ("A-t" . sp-backward-unwrap-sexp)
; ("A-v" . sp-kill-sexp)
; ("A-c" . sp-backward-kill-sexp)
; ("A-x" . sp-kill-hybrid-sexp)
; ("A-p" . sp-forward-symbol)
; ("A-o" . sp-backward-symbol)

;; Meow Spacebar Prefixes
(defvar my/leader-prefixes (make-hash-table :test #'equal)
  "Private Meow leader prefix symbols keyed by display title.")

(defun my/leader-prefix (title)
  "Return the private leader prefix symbol named TITLE."
  (or (gethash title my/leader-prefixes)
      (error "No leader prefix named %S" title)))

(defun my/leader-prefix-define (title bindings)
  "Define a private Meow leader prefix named TITLE with BINDINGS. Each element of BINDINGS is either:
  (KEY COMMAND)
  or:
  (KEY :prefix TITLE)"
  (unless (stringp title)
    (error "Leader prefix title must be a string: %S" title))
  (let ((symbol (make-symbol title))
        (map (make-sparse-keymap)))
    (dolist (binding bindings)
      (pcase binding
        (`(,key :prefix ,prefix-title)
         (unless (stringp prefix-title)
           (error "Prefix reference must be a string: %S" prefix-title))
         (keymap-set map key (my/leader-prefix prefix-title)))
        (`(,key ,command)
         (keymap-set map key command))
        (_
         (error "Invalid leader binding: %S" binding))))
    (fset symbol map)
    (puthash title symbol my/leader-prefixes)
    symbol))

(defmacro my/defleader-prefix (title &rest bindings)
  "Define a private Meow leader prefix named TITLE."
  (declare (indent 1))
  `(my/leader-prefix-define ,title ',bindings))

;; https://github.com/meow-edit/meow/blob/master/COMMANDS.org
(use-package meow
  :config
  (defun meow-setup ()
    ;; Keymap Definitions
    (my/defleader-prefix "active buffer"
      ("s" replace-string)
      ("w" delete-trailing-whitespace)
      ("i" indent-region))

    (my/defleader-prefix "eglot"
      ("a" eglot-code-actions)
      ("r" eglot-rename)
      ("f" eglot-format-buffer))

    (my/defleader-prefix "cider"
      ;; connection/session
      ("j" cider-jack-in)
      ("J" cider-jack-in-cljs)
      ("c" cider-connect-clj)
      ("C" cider-connect-cljs)
      ("q" cider-quit)
      ("r" cider-restart)
      ("s" cider-sesman-browser)

      ;; eval/load
      ("e" cider-eval-defun-at-point)
      ("E" cider-eval-last-sexp)
      ("b" cider-eval-buffer)
      ("l" cider-load-buffer)
      ("R" cider-eval-region)
      ("p" cider-pprint-eval-defun-at-point)
      ("i" cider-inspect-last-result)

      ;; repl
      ("z" cider-switch-to-repl-buffer)
      ("Z" cider-switch-to-last-clojure-buffer)
      ("x" cider-repl-clear-buffer)
      ("n" cider-repl-set-ns)

      ;; navigation/docs
      ("." cider-find-var)
      ("," cider-pop-back)
      ("d" cider-doc)
      ("D" cider-clojuredocs)
      ("a" cider-apropos)

      ;; tests/debug
      ("t" cider-test-run-test)
      ("T" cider-test-run-ns-tests)
      ("P" cider-test-run-project-tests)
      ("F" cider-test-rerun-failed-tests)
      ("g" cider-debug-defun-at-point)

      ;; refresh/refactor
      ("u" cider-ns-refresh)
      ("f" cider-format-buffer)
      ("m" cljr-rename-symbol)
      ("A" cljr-add-require-to-ns)
      ("N" cljr-clean-ns))

        (my/defleader-prefix "dev"
      ("i" consult-imenu)
      ("o" consult-outline)
      ("t" eat-project)
      ("j" majutsu)
      ("J" majutsu-log)
      ("e" :prefix "eglot")
      ("c" :prefix "cider"))

    (my/defleader-prefix "files"
      ("f" find-file)
      ("d" consult-dir)
      ("r" consult-recent-file)
      ("c" cape-file)
      ("R" rename-visited-file)
      ("D" dired-jump)    
      ("X" delete-file)    
      ("s" save-buffer))

    (my/defleader-prefix "buffers"
      ("b"  consult-buffer)
      ("X"  kill-buffer)
      ("R"  revert-buffer)
      ("s"  scratch-buffer)
      ("i"  ibuffer))

    (my/defleader-prefix "windows"
      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)
      ("v" split-window-right)
      ("s" split-window-below)
      ("a" ace-window)
      ("X" delete-window)
      ("O" delete-other-windows)
      ("=" balance-windows)
      ("w" consult-buffer-other-window)
      ("f" consult-buffer-other-frame))

    (my/defleader-prefix "tools"
      ("t" eat-project)
      ("T" eat))

    (my/defleader-prefix "project"
      ("p" project-switch-project)
      ("f" project-find-file)
      ("d" project-dired)
      ("b" consult-project-buffer))
    
    (my/defleader-prefix "search"
      ("s" consult-ripgrep)
      ("l" consult-line)
      ("L" consult-line-multi)
      ("d" xref-find-definitions)
      ("r" xref-find-references)   
      ("b" xref-go-back))

    (my/defleader-prefix "narrow"
      ("r" narrow-to-region)    
      ("f" narrow-to-defun)    
      ("w" widen))
    
    (my/defleader-prefix "help"
      ("h" helpful-at-point)
      ("k" embark-bindings))

    (meow-leader-define-key   
     (cons "a" (my/leader-prefix "active buffer"))
     (cons "d" (my/leader-prefix "dev"))
     (cons "f" (my/leader-prefix "files"))
     (cons "b" (my/leader-prefix "buffers"))
     (cons "w" (my/leader-prefix "windows"))
     (cons "t" (my/leader-prefix "tools"))
     (cons "p" (my/leader-prefix "project"))
     (cons "s" (my/leader-prefix "search"))
     (cons "n" (my/leader-prefix "narrow"))
     (cons "?" (my/leader-prefix "help"))
          
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key))
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     ;; Moving Around
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     ;; Traversal
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . meow-visit)
     '("n" . meow-search)
     ;; Negate & Reverse
     '("-" . negative-argument)
     '(";" . meow-reverse)
     ;; Making Regions
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("m" . meow-join)
     '("o" . meow-block)
     '("O" . meow-to-block)
     ;; Using Regions
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("p" . meow-yank)
     '("P" . consult-yank-pop)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("U" . meow-undo-in-selection)
     ;; Add & Subtract
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     ;; Other
     '("Q" . meow-quit)
     '("c" . comment-dwim)
     '("r" . meow-change)
     '("R" . meow-replace)
     '("/" . meow-last-buffer)
     '("s" . my/sp-wrap-menu)
     '("v" . meow-kill)
     '("u" . meow-undo)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1)
  :custom 
  (meow-use-clipboard t))

;;;; UI

(use-package frame
  :ensure nil
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (global-hl-line-mode 1)
  (column-number-mode 1)
  (line-number-mode 1)
  (show-paren-mode 1)
  (delete-selection-mode 1)
  :custom
  (show-paren-delay 0.0)
  (line-spacing 1)
  (x-underline-at-descent-line nil)
  (indicate-buffer-boundaries 'left)
  (display-line-numbers-width 3))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
	'( :internal-border-width 0
           :header-line-width 0
           :mode-line-width 6
           :tab-width 0
           :right-divider-width 5
           :scroll-bar-width 0
           :fringe-width 0))
  (setq spacious-padding-subtle-frame-lines nil)
  (spacious-padding-mode 1))

(use-package ultra-scroll
  ;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)    
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Customize later: https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :after nerd-icons
  :config
  (setq dashboard-items '((projects  . 6)
			  (registers . 6)
			  (recents   . 15)
                          (bookmarks . 15)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (add-hook 'server-after-make-frame-hook 'dashboard-open)
  (setq initial-buffer-choice 'dashboard-open)
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;;;; Themes

(setq modus-themes-italic-constructs t)

(load-theme 'modus-vivendi-tinted t)

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;;; Fonts

(setq inhibit-compacting-font-caches t)

(defvar my/default-font-height 180)

(defun my-set-font-size (&optional frame)
  "Set font size for FRAME."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (set-face-attribute 'default frame :height my/default-font-height))))

(add-hook 'after-make-frame-functions #'my-set-font-size)

(when (display-graphic-p)
  (my-set-font-size))

;;;; File Management

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package files
  :ensure nil
  :custom
  (create-lockfiles nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (delete-by-moving-to-trash t))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode 1)
  :custom
  ;; Some systems don't do file notifications well: https://todo.sr.ht/~ashton314/emacs-bedrock/11
  ;(auto-revert-avoid-polling t)
  (auto-revert-avoid-polling nil)
  (auto-revert-interval 5)
  (auto-revert-check-vc-info t))

;;;; Garbage Collector Magic Hack
(use-package gcmh
  :config
  (gcmh-mode t))

(use-package ace-window
  :bind
  ("s-/" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; TODO: Consider using Frames Only Mode later
;; https://github.com/davidshepherd7/frames-only-mode
;; (use-package frames-only-mode)

;;;; Files

;; Batch Editing Directories & Files:
;; https://www.youtube.com/watch?v=1E0ThCSr8Qw
(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-vc-rename-file t))

(use-package dirvish
  :after dired
  :config
  (dirvish-override-dired-mode))

;;;; Emacs Core Packages

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project")))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))  ; Ediff uses the existing frame

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package text-mode
  :ensure nil
  :hook
  (text-mode . visual-line-mode))

(use-package minibuffer
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  ;; Disables caps sensitivity across searches
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  ;; Hide commands in M-x which do not work in the current mode. Vertico
  ;; commands are hidden in normal buffers. This is useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package emacs
  :ensure nil
  :init
  (setq inhibit-compacting-font-caches t)
  (setq use-short-answers t)
  (setq inhibit-startup-message t)
  :config
  (windmove-default-keybindings 'super)
  (context-menu-mode t)
  (when (display-graphic-p)
    (context-menu-mode))
  :custom
  ;; Fix archaic defaults
  (sentence-end-double-space nil)
  (apropos-do-all t) ;; Apropos commands perform more extensive searches than default.
  (load-prefer-newer t) ;; Prevents stale elisp bytecode
  ;; UI tweaks
  (switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent
  (show-trailing-whitespace nil))

;;;; TODO

;; Magit

;; Magit-Todos

;; Magit-Delta

;; Diff-hl

;; Anzu
