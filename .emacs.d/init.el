;; -*- lexical-binding: t; -*-

;;;; Bootstrapping Elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
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
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package avy
  :init
  (avy-setup-default)
  :config
  (defun avy-goto-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "(+")))
  :bind
  (("A-a" . 'avy-goto-char-timer)
   ("A-s" . 'avy-goto-parens))
  :custom
  (avy-timeout-seconds 0.8))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind 
  (:map minibuffer-local-map
    ("M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; https://github.com/oantolin/embark/
(use-package embark
  :bind
  (("A-Q" . embark-act)         ;; pick some comfortable binding
   ("A-W" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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
  ;; TODO: Replace bindings.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c h" . consult-history)
         ("H-b" . consult-buffer)                ;; orig. switch-to-buffer
         ;("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("H-f" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("H-p" . consult-project-buffer)            ;; orig. project-switchs-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)                ;; orig. abbrev-prefix-mark (unrelated)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                      ;; orig. yank-pop
	 ("A-/" . consult-ripgrep)
         ("M-s l" . consult-line))                       ;; needed by consult-line to detect isearch
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
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point))

;; TODO: set up Corfu
;; https://github.com/meatcar/emacs.d
;; https://github.com/minad/corfu
(use-package corfu)

;; TODO: Set up after Corfu
;; https://kristofferbalintona.me/posts/202203130102/
;; https://www.youtube.com/watch?v=Vx0bSKF4y78&t=768s
(use-package cape)

;; TODO integrate Emacs into Window Buffer


;;;; Development

;; Setup: https://github.com/karthink/gptel
(use-package gptel)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; Clojure

(use-package clojure-mode)

(use-package cider
  :hook (clojure-mode . cider-mode))

(use-package clj-refactor
  :after cider
  :hook (clojure-mode . clj-refactor-mode))

;;(use-package add-node-modules-path)

;; Markdown

;;(use-package markdown-mode
  ;:mode ("README\\.md\\'" . gfm-mode)
  ;:init (setq markdown-command "multimarkdown")
  ;:bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))


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

;; Great Smartparens guide: 
;; https://ebzzry.com/en/emacs-pairs/
(use-package smartparens
  :hook 
  ((prog-mode . smartparens-mode)
   (text-mode . smartparens-mode)
   (markdown-mode . smartparens-mode))
  ;;(prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config)
  :bind
  (;; Traversal
   ("A-l" . sp-beginning-of-sexp)
   ("A-;" . sp-end-of-sexp)
   ("A-k" . sp-forward-sexp)
   ("A-j" . sp-backward-sexp)
   ("A-m" . sp-next-sexp)
   ("A-n" . sp-previous-sexp)
   ("A-p" . sp-forward-symbol)
   ("A-o" . sp-backward-symbol)
   ("A-h" . sp-down-sexp)
   ("A-b" . sp-backward-down-sexp)
   ("A-i" . sp-up-sexp)
   ("A-u" . sp-backward-up-sexp)
   ;; Manipulation
   ;; Consider Wrapping Fn: https://ebzzry.com/en/emacs-pairs/
   ("s-m" . sp-unwrap-sexp)
   ("s-n" . sp-backward-unwrap-sexp)
   ("s-j" . sp-forward-slurp-sexp)
   ("s-h" . sp-backward-slurp-sexp)
   ("s-l" . sp-forward-barf-sexp)
   ("s-k" . sp-backward-barf-sexp)
   ("s-b" . sp-transpose-sexp) ; Swapping symbols
   ("s-o" . sp-kill-sexp)
   ("s-i" . sp-backward-kill-sexp)
   ("s-p" . sp-kill-hybrid-sexp)))


;; https://github.com/meow-edit/meow/blob/master/COMMANDS.org
(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
      '("j" . meow-next)
      '("k" . meow-prev)
      '("<escape>" . ignore))
    (meow-leader-define-key
      ;; SPC j/k will run the original command in MOTION state.
      '("j" . "s-j")
      '("k" . "s-k")
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
      '("/" . meow-keypad-describe-key)
      '("?" . meow-cheatsheet))
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
      '("n" . meow-search)
      ;; Negate & Reverse
      '("-" . negative-argument)
      '(";" . meow-reverse)
      ;; Making Regions
      '("," . meow-inner-of-thing)
      '("." . meow-bounds-of-thing)
      '("[" . meow-beginning-of-thing)
      '("]" . meow-end-of-thing)
      ;; Using Regions
      '("g" . meow-cancel-selection)
      '("G" . meow-grab)
      ;; Add & Subtract
      '("a" . meow-append)
      '("A" . meow-open-below)
      '("i" . meow-insert)
      '("I" . meow-open-above)
      '("c" . kill-region)
      '("d" . meow-delete)
      '("D" . meow-backward-delete)
      ;; IDK
      '("m" . meow-join)
      '("o" . meow-block)
      '("O" . meow-to-block)
      '("p" . consult-yank-from-kill-ring)
      '("q" . meow-quit)
      '("Q" . meow-goto-line)
      '("r" . meow-replace)
      '("R" . meow-swap-grab)
      '("s" . meow-kill)
      '("t" . meow-till)
      '("u" . meow-undo)
      '("U" . meow-undo-in-selection)
      '("v" . meow-visit)
      '("w" . meow-mark-word)
      '("W" . meow-mark-symbol)
      '("x" . meow-line)
      '("X" . meow-goto-line)
      '("y" . meow-save)
      '("Y" . meow-sync-grab)
      '("z" . meow-pop-selection)
      '("'" . repeat)
      '("<escape>" . ignore)))
      (meow-setup)
      (meow-global-mode 1)
      :custom 
      (meow-use-clipboard t))


;;;; Nice to Haves

(use-package no-littering
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)    
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Anzu

;; Customize later: https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  ;; New: Open dashboard in new client frames
  (add-hook 'server-after-make-frame-hook 
            (lambda () (switch-to-buffer dashboard-buffer-name)))
  (dashboard-setup-startup-hook))

(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (add-to-list 'display-buffer-alist
               '("*[Hh]elp"
                 (display-buffer-reuse-mode-window
                  display-buffer-pop-up-window))))

(use-package ultra-scroll
  ;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;; Garbage Collector Magic Hack
(use-package gcmh
  :config
  (gcmh-mode t))

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


;; TODO: Frames Only Mode
;; https://github.com/davidshepherd7/frames-only-mode
(use-package frames-only-mode)

;;;; Version Control

;; Magit

;; Magit-Todos

;; Magit-Delta

;; Diff-hl

;; Jujutsu?

;;;; Emacs

(use-package emacs
  :ensure nil
  :init
  ;; Set Hotkeys 
  (setq ns-right-option-modifier 'super)
  (setq ns-right-control-modifier 'alt)
  (remove-alt-default-bindings)
  ;; Startup Performance
  (setq inhibit-compacting-font-caches t)
  ;; Manage Backup Files
  (global-auto-revert-mode 1)
  (setq create-lockfiles nil)
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  ;; life is too short to type yes or no
  (setq use-short-answers t)
  ;; clean up dired buffers
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; Disable startup-screen, tool bar, menu bar, scroll bar.
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
					;(scroll-bar-mode -1)
  ;; Highlight current line.
  (global-hl-line-mode t)
  ;; Shows matching parens
  (show-paren-mode t)
  (setq show-paren-delay 0.0)
  ;; Scales the system font size to get emacs font size. 
  ;;(set-face-attribute 'default nil :height (floor (* (face-attribute 'default :height) 1.4)))

  ;; Font size configuration
  (defun my-set-font-size (&optional frame)
    "Set font size for FRAME (or current frame if nil)."
    (let ((target-frame (or frame (selected-frame))))
      (when (display-graphic-p target-frame)
	(with-selected-frame target-frame
          (set-face-attribute 'default target-frame :height (floor (* (face-attribute 'default :height) 1.5)))))))

  ;; Register hook for new frames (including emacsclient)
  (add-hook 'after-make-frame-functions #'my-set-font-size)

  ;; Apply immediately for regular Emacs startup
  (when (display-graphic-p)
    (my-set-font-size))

  
  ;; Themes
  ;; https://protesilaos.com/emacs/modus-themes#h:bf1c82f2-46c7-4eb2-ad00-dd11fdd8b53f
  (load-theme 'modus-vivendi-tinted) ; Prot's dark theme
  (setq-default line-spacing 1)
  (setq modus-themes-italic-constructs t)
  :config
  ;; Bedrock
  ;; Move through windows with Ctrl-<arrow keys>
  (windmove-default-keybindings 'control)
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer to switch display modes.
  (context-menu-mode t)
  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))
  ;; Cursor doesn't blink
  (blink-cursor-mode -1)
  ;; Show current line in modeline
  (line-number-mode t)
  :hook
  ;; Display line numbers in programming mode
  (prog-mode . display-line-numbers-mode)
  ;; Nice line wrapping when working with text
  (text-mode . visual-line-mode)
  :custom
  ;;; Bedrock
  ;; Automatically reread from disk if the underlying file changes
  (auto-revert-avoid-polling t)
  ;; Some systems don't do file notifications well; see https://todo.sr.ht/~ashton314/emacs-bedrock/11
  (auto-revert-interval 5)
  (auto-revert-check-vc-info t)
  ;; Fix archaic defaults
  (sentence-end-double-space nil)
  (enable-recursive-minibuffers t)
  (apropos-do-all t) ;; Apropos commands perform more extensive searches than default.
  (load-prefer-newer t) ;; Prevents stale elisp bytecode
  (ediff-window-setup-function 'ediff-setup-windows-plain) ;; Ediff uses the existing frame
					;(add-to-list 'project-vc-root-markers ".project" 'append)
  (project-vc-extra-root-markers '(".project"))
  ;; UI tweaks
  (column-number-mode t) ; Show column as well
  (x-underline-at-descent-line nil) ; Prettier underlines
  (switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent
  (show-trailing-whitespace nil) ; By default, don't underline trailing spaces
  (indicate-buffer-boundaries 'left) ; Show buffer top and bottom in the margin
  (display-line-numbers-width 3) ; Set a minimum width for line numbers
  ;; Disables caps sensitivity across searches
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  ;; Hide commands in M-x which do not work in the current mode. Vertico
  ;; commands are hidden in normal buffers. This is useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))
