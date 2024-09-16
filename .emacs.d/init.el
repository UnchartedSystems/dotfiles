
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

;;;; Start Init

;; which-key
(use-package which-key :ensure t :demand t
	:config (which-key-setup-side-window-bottom))

;; meow
(use-package meow :ensure t :demand t
  :config 
  (defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
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
  (meow-global-mode 1))

;;; TODO Fix Magit (w compat?)
;; compat
;(use-package compat :ensure t :demand t)

;; Magit
;(use-package magit :ensure t :demand t
  ;:after (compat))

;;; NOTE: the next few packages are from here: 
;;; https://lambdaland.org/posts/2024-05-30_top_emacs_packages/

;; Avy
(use-package avy :ensure t :demand t
  :config
  ; TODO: Configure these here: https://github.com/abo-abo/avy
  ;(global-set-key (kbd "C-'") 'avy-goto-char)
  (global-set-key (kbd "C-:") 'avy-goto-char-timer)
  ;(global-set-key (kbd "M-g w") 'avy-goto-word-1)
  ;(global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

; TODO NOTE: Revisit this to superpower Avy Usage:
; https://karthinks.com/software/avy-can-do-anything/#dot-dot-dot-avy-s-documentation-leaves-out-the-best-part

;; Marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

; Consult

(use-package consult
  :ensure t
  :demand t)

; Embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))



;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


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
  :init 
    (global-corfu-mode)
    (setq corfu-auto t))

;; Popper
(use-package popper
  :ensure t ; or :straight t
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

;; Jinx

;; Eat

;;;; Clojure

;; Cider
(use-package cider
  :ensure t
  :demand t)

;;;; Theming

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
;    (setq doom-themes-enable-bold t  ; if nil, bold is universally disabled
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
  :init (doom-modeline-mode 1))

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
             :default-height 100
             :variable-pitch-family "Iosevka Comfy Duo")
            (regular) ; like this it uses all the fallback values and is named `regular'
            (medium
             :default-weight semilight
             :default-height 130
             :bold-weight extrabold)
            (large
             :inherit medium
             :default-height 160)
            (presentation
             :default-height 200)
            (t
             ;; I keep all properties for didactic purposes, but most can be
             ;; omitted.  See the fontaine manual for the technicalities:
             ;; <https://protesilaos.com/emacs/fontaine>.
             :default-family "Iosevka Comfy"
             :default-weight regular
             :default-height 100

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

;;;; Emacs Config
(use-package emacs
  :ensure nil
  :demand t
  :config
    (setq scroll-step 0)
    (setq scroll-conservatively 101)
  :custom
    ;;;; Theming
    ;; Remove Title Bar
    ;(add-to-list 'default-frame-alist '(undecorated . t))
    ;; Remove Welcome Screen
    (inhibit-startup-screen t)
    (menu-bar-mode nil)
    (tool-bar-mode nil)
    (scroll-bar-mode nil)
    (frame-resize-pixelwise t)

    ;;;; Vertico
    ;; Support opening new minibuffers from inside existing minibuffers.
    (enable-recursive-minibuffers t)
    ;; Hide commands in M-x which do not work in the current mode.  Vertico
    ;; commands are hidden in normal buffers. This setting is useful beyond
    ;; Vertico.
    (read-extended-command-predicate #'command-completion-default-include-p)
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
    (setq minibuffer-prompt-properties
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
