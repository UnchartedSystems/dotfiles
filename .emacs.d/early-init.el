;;; -*- lexical-binding: t; -*-

;;; Fixes native-comp error for MacPorts Emacs.app
;; TODO: set this as conditional on MacOS?
(setenv "LIBRARY_PATH" "/opt/local/lib/gcc15:/opt/local/lib/libgcc:/opt/local/lib/gcc15/gcc/aarch64-apple-darwin24/15.2.0")

;; Example Elpaca early-init.el -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq inhibit-compacting-font-caches t)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(provide 'early-init)
