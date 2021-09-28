;;; +tools.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Setup various tools.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;
;; Packages

(use-package dired
  :straight nil
  :init
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  ;; enable extensions like C-x C-j(dired-jump)
  (require 'dired-x))


;; Git
(use-package magit
  :ensure t
  :bind (:prefix "C-c g"
         :prefix-map magit-map
         :prefix-docstring "Magit"
            ("g" . magit-status)
            ("i" . magit-init)
            ("S" . magit-stage-file)
            ("U" . magit-unstage-file)
            ("b" . magit-blame-addition))
  :init
  ;; Prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (expand-file-name "transient/levels" user-emacs-cache-directory)
        transient-values-file  (expand-file-name "transient/values" user-emacs-cache-directory)
        transient-history-file (expand-file-name "transient/history" user-emacs-cache-directory)))


;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp-install-server
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-session-file (expand-file-name "lsp-session" user-emacs-cache-directory)
        lsp-server-install-dir (expand-file-name "lsp/" user-emacs-cache-directory))
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumbs-enable nil)
  ;; Keep workspace alive when the last workspace buffer is closed
  (setq lsp-keep-worksapce-alive t)
  ;;
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  ;; Disable some features that might slowdown
  (setq lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-text-document-color nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references)
            ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-enable nil)
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))


;;  Debug Adapter Protocol
(use-package dap-mode
  :ensure t
  :config
  (setq dap-print-io t))

(use-package dap-ui
  :straight nil
  :after dap-mode
  :hook (dap-mode-hook . dap-ui-mode)
  :config
  (setq dap-auto-configure-features '(locals sessions tooltip)))


;; Vterm
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map;
         ("C-S" . isearch-repeat-forward)
         ("C-R" . isearch-repeat-backward)))


;; Project interaction
(use-package projectile
  :ensure t
  :bind (:prefix "C-x p"
         :prefix-map projectile-map
         :prefix-docstring "Projectile"
            ("o" . projectile-switch-project)
            ("f" . projectile-find-file)
            ("d" . projectile-dired)
            ("i" . projectile-project-info)
            ("g" . projectile-grep)
            ("s" . projectile-switch-project)
            ("e" . projectile-run-vterm))
  :init
  (setq projectile-project-search-path '("~/projects/" "~/src/")
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-cache-directory))
  :config
  (projectile-mode +1))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)


;; Navigation to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char)
         ("s-:" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 1
        avy-background t))


;; Syntax checking on the fly
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-eldev
  :ensure t)


;; Kubernetes
(use-package kubel
  :ensure t)


(provide '+tools)
;;; +tools.el ends here.
