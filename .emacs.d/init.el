;;;; Bootstrapping Elpaca

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks (windows):
;; (elpaca-no-symlink-mode)

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;;; Useful Packages

;; which-key
(use-package which-key :ensure t :demand t
  :config (which-key-setup-side-window-bottom))

;; meow
(defun nt-wrap-string () (interactive) (sp-wrap-with-pair "\""))
(use-package meow :ensure t :demand t
  :config 
  (defun meow-setup ()
    (setopt meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
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
     ;; Basic Traversal
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     ;; Negation & Reversal
     '("'" . negative-argument)
     '("\"" . meow-reverse)
     ;; Making region of [thing]
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     ;; Normal -> Insert
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("c" . meow-change)
     ;; Create Region by: [word]
   ;;; -> NOTE: back removed using negative
     '("e" . meow-next-word)
     '("E" . meow-back-word)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     ;; Create Region by: [block]
     '("o" . meow-block)
     '("O" . meow-to-block)
     ;; Create Region by [Search]
     '("f" . meow-till)
     '("F" . meow-find)
     '("v" . meow-visit)
     '("V" . meow-goto-line) 
     ;; Working with lines
     '("x" . meow-line)
     '("X" . meow-join)
     ;; Delete
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("s" . meow-kill)
     ;; Replace (Paste)
     '("r" . meow-replace)
     ;; Cycle Selection Instance
     '("/" . meow-search)
     ;; Remove Mark
     '("g" . meow-cancel-selection)
     ;; Enter Beacon Mode
     '("G" . meow-grab)
     ;; Yank
     '("p" . meow-yank)
     ;; Undo
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     ;; Repeat
     '(";" . repeat)
     ;; Grab, Swap, Sync, & Pop - TODO: WTF are these?
     '("R" . meow-swap-grab)
     '("Y" . meow-sync-grab)
					;'("z" . meow-pop-selection)
     ;; Emacs Integration
     '("q" . meow-quit)
     '("Q" . quit-window)
     '("y" . meow-save)
     '("A-r" . meow-last-buffer)
     '("<escape>" . ignore)
   ;;;; Meow
     ;; Visual Mode? (retain region?)
   ;;;; Clojure Mode Test
     ;;'("=" . clojure-align) ; NOTE: 'internal error'? Cider is better, uses cljfmt
   ;;;; Smartparens
     ;; Forward & Backward - With Negative
     '("b" . sp-forward-sexp)
     '("B" . sp-backward-sexp)
     ;; Up & Down
     '("n" . sp-backward-down-sexp)
     '("m" . sp-down-sexp)
     '("N" . sp-backward-up-sexp)
     '("M" . sp-up-sexp)
     ;; Wrap Menu
     '("t s" . sp-wrap-square)
     '("t r" . sp-wrap-round)
     '("t c" . sp-wrap-curly)
     '("t g" . nt-wrap-string)   
     ;; Unwrap
     '("T" . sp-unwrap-sexp) 
     ;; Slurp
     '("[" . sp-backward-slurp-sexp) 
     '("]" . sp-forward-slurp-sexp) 
     ;; Barf
     '("{" . sp-backward-barf-sexp) 
     '("}" . sp-forward-barf-sexp) 
     ;; Kill
     '("‘" . sp-backward-kill-sexp) ; A-[
     '("’" . sp-kill-sexp) ; A-]
     ;; Absorb & Transpose
     '("“" . sp-absorb-sexp))) ; A-{
  '("”" . sp-transpose-sexp) ;A-}
  (meow-setup)
  (meow-global-mode 1))


;; Magit
(use-package magit
  :ensure t
  :demand t
  :bind
  ("A-g" . magit-status))

;; Transient - Needed as a Workaround for Magit
(use-package transient :ensure t :demand t)

;;; NOTE: the next few packages are from here: 
;;; https://lambdaland.org/posts/2024-05-30_top_emacs_packages/

;; Avy
(use-package avy :ensure t :demand t
  :init
  (avy-setup-default)
  :config
  (defun avy-goto-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "(+")))
  :bind
  (("A-a" . 'avy-goto-char-timer)
   ("A-n" . 'avy-goto-parens))
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; TODO: Unify this with Ace Window!
  (avy-timeout-seconds 0.8)
  (avy-all-windows t))

					; TODO NOTE: Revisit this to superpower Avy Usage:
					; https://karthinks.com/software/avy-can-do-anything/#dot-dot-dot-avy-s-documentation-leaves-out-the-best-part

;; ag - The Silver Searcher
(use-package ag
  :ensure t
  :demand t
  :custom
  (ag-highlight-search t)
  :bind
  (("A-/" . ag)))

;; Marginalia
(use-package marginalia
  :ensure t
  :demand t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-a" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

					; Consult
(use-package consult
  :ensure t
  :demand t
  :bind
  (("A-b" . consult-buffer)
   ("A-B" . ibuffer)
   ("A-k" . consult-yank-from-kill-ring)
   ("A-P" . consult-project-buffer)
   ("A-p" . project-find-file)
   ("A-f" . consult-recent-file)
   ;; TODO: Try to consolidate line fns into 1 compound consult fn
   ("A-l" . consult-line)
   ("A-L" . consult-goto-line)))

					; Embark
(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setopt prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the mode line to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setopt eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Embark Integration for Consult
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point))

;; Vertico
(use-package vertico
  :ensure t
  :demand t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

					; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Orderless
(use-package orderless
  :ensure t
  :demand t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Corfu
(use-package corfu
  :ensure t
  :demand t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :ensure t
  :demand t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Popper
(use-package popper
  :ensure t ; or :straight t
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setopt popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            help-mode
            compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package simpleclip
  :ensure t
  :demand t
  :bind
  (("A-x" . simpleclip-cut)
   ("¢" . simpleclip-copy) ; A-c
   ("A-v" . simpleclip-paste))
  :config (simpleclip-mode t))

;; Ace Window

(use-package ace-window
  :ensure t
  :defer 1
  :bind
  ("A-w" . 'ace-window)
  :config
					; (set-face-attribute
					;  'aw-leading-char-face nil
					;  :foreground "deep sky blue"
					;  :weight 'bold
					;  :height 3.0)
					; (set-face-attribute
					;  'aw-mode-line-face nil
					;  :inherit 'mode-line-buffer-id
					;   :foreground "lawn green")
  (setopt aw-dispatch-always t
					;default cursor-in-non-selected-windows 'hollow
	  aw-scope 'frame
	  aw-reverse-frame-list t
	  aw-keys '(?a ?s ?d ?f ?j ?k ?l)
	  aw-dispatch-alist
	  '((?q aw-delete-window "Ace - Delete Window")
	    (?c aw-copy-window "Ace - Copy Window")
	    (?m aw-swap-window "Ace - Swap Window")
	    (?n aw-flip-window "Ace - Flip Window")
	    (?v aw-split-window-vert "Ace - Split Vert Window")
	    (?h aw-split-window-horz "Ace - Split Horz Window")
	    (?g delete-other-windows "Ace - Maximize Window")
	    (?b balance-windows)
	    (?u (lambda ()
		  (progn
		    (winner-undo)
		    (setopt this-command 'winner-undo))))
	    (?r winner-redo))))

;; Jinx

;; Eat

;; LSP support

(use-package lsp-mode
  :ensure t
  :after clojure-mode
  :bind
  ;;;; TODO: Ensure these only run for clojure lsp mode!!
  ("H-d" . lsp-find-definition)
  ("H-r" . lsp-ui-peek-find-references)
  ("H-R" . lsp-rename)
  ;;;; TODO: fix lsp workspace symbol for Vertico... Looks very useful!!
  ;; ("H-p" . lsp-ui-find-workspace-symbol)

  ;;;; NOTE: there are a lot more!
  ;; https://clojure-lsp.io/features/#find-a-functionvar-definition
  :hook
  ((clojure-mode . lsp-mode)
   (clojurec-mode . lsp-mode)
   (clojurescript-mode . lsp-mode))
  :custom
  (lsp-completion-provider :none)
  :init
  (defun my/lsp-mode-setup-completion () ; from the Corfu Wiki
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;; (setopt lsp-clojure-server-command '("/usr/local/bin/clojure-lsp"));; Optional: In case `clojure-lsp` is not in your $PATH
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)) 

(use-package lsp-ui
  :ensure t
  :demand t
  :after lsp-mode
  :commands lsp-ui-mode)

;; SmartParens
(use-package smartparens
  :ensure t
  :demand t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; Rainbow-Delimiters
;(use-package rainbow-delimiters
;  :ensure t
;  :demand t
;  :hook
;  (prog-mode . rainbow-delimiters-mode))

;; Aggressive-Indent
(use-package aggressive-indent
  :ensure t
  :demand t
  :hook
  (prog-mode . aggressive-indent-mode))

;;;; Clojure

;; Clojure-Mode
(use-package clojure-mode
  :ensure t
  :demand t
  :custom
  (clojure-align-forms-automatically t)
  :catch (lambda (keyword err) (message (error-message-string err))))

;; Cider
(use-package cider
  :ensure t
  :demand t
  :after clojure-mode
  :bind
  (("H-c" . cider-repl-handle-shortcut)))

;;;; Theme & Font

(use-package modus-themes
  :ensure t
  :demand t
  :custom
    ;;;; Basic Config
  ;; In all of the following, WEIGHT is a symbol such as `semibold',
  ;; `light', `bold', or anything mentioned in `modus-themes-weights'.
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)

  ;; Options for `modus-themes-prompts' are either nil (the
  ;; default), or a list of properties that may include any of those
  ;; symbols: `italic', `WEIGHT'
  (modus-themes-prompts '(italic bold))

  ;; The `modus-themes-completions' is an alist that reads two
  ;; keys: `matches', `selection'.  Each accepts a nil value (or
  ;; empty list) or a list of properties that can include any of
  ;; the following (for WEIGHT read further below):
  ;;
  ;; `matches'   :: `underline', `italic', `WEIGHT'
  ;; `selection' :: `underline', `italic', `WEIGHT'
  (modus-themes-completions
   '((matches . (extrabold))
     (selection . (semibold italic text-also))))

  (modus-themes-org-blocks 'gray-background) ; {nil,'gray-background,'tinted-background}

  ;; The `modus-themes-headings' is an alist: read the manual's
  ;; node about it or its doc string.  Basically, it supports
  ;; per-level configurations for the optional use of
  ;; `variable-pitch' typography, a height value as a multiple of
  ;; the base font size (e.g. 1.5), and a `WEIGHT'.
  (modus-themes-headings
   '((1 . (variable-pitch 1.5))
     (2 . (1.3))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.1))))

    ;;;; Advanced Config
  (modus-themes-common-palette-overrides
   '((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))

  ;; Remember that more (MUCH MORE) can be done with overrides, which we
  ;; document extensively in the modus-themes manual.
  ;; https://protesilaos.com/emacs/modus-themes
  :config    
  (load-theme 'modus-vivendi-tinted))

(use-package doom-themes
  :ensure t
  :demand t
					;  :config
					;    (setopt doom-themes-enable-bold t  ; if nil, bold is universally disabled
					;        doom-themes-enable-italic t) ; if nil, italics is universally disabled
					;    (doom-themes-visual-bell-config)
					;    (load-theme 'doom-snazzy t)
  )

(use-package nerd-icons
  :ensure t
  :demand t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-install-fonts)
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t
  :demand t
  :after nerd-icons
  :init (doom-modeline-mode 1)
					;  :custom
					;  (doom-modeline-height 35)
  :config
  ;; TODO: Set Modeline face font using Fontaine for Fontaine functionality!
  (custom-set-faces
   '(mode-line ((t (:height 1.1))))
   '(mode-line-active ((t (:height 1.1)))) ; For 29+
   '(mode-line-inactive ((t (:height 1.1)))))
  )

;;;; Fonts
;; Amazing explanation of vanilla font config & Fontaine by Prot
;; https://www.youtube.com/watch?v=qR8JRYr4BKE
(use-package fontaine
  :ensure t
  :demand t
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://github.com/protesilaos/iosevka-comfy>.
  (fontaine-presets
   '((small
      :default-family "Iosevka Comfy Motion"
      :default-height 140
      :variable-pitch-family "Iosevka Comfy Duo")
     (regular) ; like this it uses all the fallback values and is named `regular'
     (medium
      :default-weight semilight
      :default-height 160
      :bold-weight extrabold)
     (large
      :inherit medium
      :default-height 180)
     (presentation
      :default-height 220)
     (t
      ;; I keep all properties for didactic purposes, but most can be
      ;; omitted.  See the fontaine manual for the technicalities:
      ;; <https://protesilaos.com/emacs/fontaine>.
      :default-family "Iosevka Comfy"
      :default-weight regular
      :default-height 150

      :fixed-pitch-family nil ; falls back to :default-family
      :fixed-pitch-weight nil ; falls back to :default-weight
      :fixed-pitch-height 1.0

      :fixed-pitch-serif-family nil ; falls back to :default-family
      :fixed-pitch-serif-weight nil ; falls back to :default-weight
      :fixed-pitch-serif-height 1.0

      :variable-pitch-family "Iosevka Comfy Motion Duo"
      :variable-pitch-weight nil
      :variable-pitch-height 1.0

      :mode-line-active-family nil ; falls back to :default-family
      :mode-line-active-weight nil ; falls back to :default-weight
      :mode-line-active-height 0.9

      :mode-line-inactive-family nil ; falls back to :default-family
      :mode-line-inactive-weight nil ; falls back to :default-weight
      :mode-line-inactive-height 0.9

      :header-line-family nil ; falls back to :default-family
      :header-line-weight nil ; falls back to :default-weight
      :header-line-height 0.9

      :line-number-family nil ; falls back to :default-family
      :line-number-weight nil ; falls back to :default-weight
      :line-number-height 0.9

      :tab-bar-family nil ; falls back to :default-family
      :tab-bar-weight nil ; falls back to :default-weight
      :tab-bar-height 1.0

      :tab-line-family nil ; falls back to :default-family
      :tab-line-weight nil ; falls back to :default-weight
      :tab-line-height 1.0

      :bold-family nil ; use whatever the underlying face has
      :bold-weight bold

      :italic-family nil
      :italic-slant italic

      :line-spacing nil)))
  :config
  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  
  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)
  
  ;; fontaine does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

;;;; Keybinds Philosophy (via reddit)
					; Use Hyper as a namespace for your personal bindings per mode.
					; Use Alt as a namespace for you personal bindings that are global.
					; Do not customize a single binding in any other namespace.
					; Your Emacs life will be so simple after that.
					; You will use this approach for the rest of your life.

;; TODO: Bind Hyper to a key for use with meow-keypad
;; Bind useful functions within Hyper and Alt

;;;; Emacs Config
(use-package emacs
  :ensure nil
  :demand t
  :bind
  (("A-e" . other-window)
   ("A-F" . find-file)
   ("A-s" . save-buffer)
   ("A-q" . kill-buffer)
   ("A-Q" . delete-window))
  :config
  (defun bedrock--backup-file-name (fpath)
    "Return a new file path of a given file path. If the new path's directories does not exist, create them."
    (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
           (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
           (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))
  (setopt make-backup-file-name-function 'bedrock--backup-file-name)
  :custom
  ;;;; Bedrock
  ;; Automatically reread from disk if the underlying file changes
  (auto-revert-avoid-polling t)
  ;; Some systems don't do file notifications well; see
  ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
  (auto-revert-interval 5)
  (auto-revert-check-vc-info t)
  (global-auto-revert-mode)
  (sentence-end-double-space nil)
  (make-backup-file-name-function 'bedrock--backup-file-name)
  (switch-to-buffer-obey-display-actions t)
  (line-number-mode t)
  (column-number-mode t)
  (blink-cursor-mode -1)
  ;;;; Backup Files
  (setq lock-file-name-transforms
	'(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/.temp/\\1" t)))
  (setq auto-save-file-name-transforms
	'(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/.temp/\\1" t)))
  (setq backup-directory-alist
	'((".*" . "~/.emacs.d/.temp/")))
  ;;;; Theming
  (inhibit-startup-screen t)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (frame-resize-pixelwise t)
  (pixel-scroll-precision-mode t)
  (project-vc-extra-root-markers '(".project"))
  ;;;; Scrolling
  (scroll-margin 5)
  (scroll-conservatively 101)
  ;;;; Consult
  ;; recentf-mode is used for recent file history
  (recentf-mode t)
  ;;;; Corfu
  ;; TAB cycle if there are only few candidates
					; (completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (tab-always-indent 'indent)
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
    ;;;; Vertico
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :hook
  ((aggressive-indent . emacs-lisp-mode-hook)
  ;;;; Bedrock
   (prog-mode . display-line-numbers-mode)
   (visual-line-mode . text-mode-hook))
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Generated By Emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d146df8bd640320d5ca94d2913392bc6f763d5bc2bb47bed8e14975017eea91" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


