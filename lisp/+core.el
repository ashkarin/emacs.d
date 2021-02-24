;;; +core.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file installs and configures core packages.

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


;;; Built-in packages

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

;; sets unique names for buffer dependent on file name
(use-package uniquify
  :straight nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; ignore special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; remembers location in files when saving
(use-package saveplace
  :straight nil
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-cache-directory))
  ;; activate it for all buffers
  (setq-default save-place t))

;; loads the previous minibuffer histories from `savehist-file'
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-emacs-cache-directory))
  (savehist-mode +1))

;; interactivly evaluate Emacs Lisp expressions
(use-package ielm
  :straight nil
  :hook ((ielm-mode . eldoc-mode)))

;; insertions matching delimiters
(use-package elec-pair
  :straight nil
  :config
  (electric-pair-mode +1))


;;; Third-party packages

(use-package magit
  :ensure t
  :init
  ;; Prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (expand-file-name "transient/levels" user-emacs-cache-directory)
        transient-values-file  (expand-file-name "transient/values" user-emacs-cache-directory)
        transient-history-file (expand-file-name "transient/history" user-emacs-cache-directory))
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

;; project interaction
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/projects/" "~/work/")
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-cache-directory))
  :bind (("s-p"     . 'projectile-command-map)
         ("C-c C-p" . 'projectile-command-map))
  :config
  (projectile-mode +1))

;; interface for selecting items from a list
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

;; sorting and filtering for lists of candidates
(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; modular in-buffer completion mechanism
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-global-modes '(not erc-mode help-mode gud-mode))
  (global-company-mode))

;; on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-eldev
  :ensure t)

;; on-the-fly spell checking
(use-package flyspell
  :hook ((text-mode      . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")))

;; navigation to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char)
         ("s-:" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 1
        avy-background t))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

;; displays the key bindings following currently
;; entered incomplete command
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; auto-save buffers, when certain events happen
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

;; navigation for imenu tags across all buffers that
;; satisfy a filtering criteria
(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

;; deals with pairs (e.g. insert, unwrap, navigate, etc.)
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  ;; As smartparens scans are relatively expensive
  ;; reduce it for a better performance.
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4))


;; bundle of useful interactive commands
(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("s-r"   . crux-recentf-find-file)
         ("C-c f" . crux-recentf-find-file)
         ("C-c e" . crux-eval-and-replace)
         ("C-c u" . crux-view-url)
         ("C-c w" . crux-swap-windows)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c s" . crux-ispell-word-then-abbrev)
         ("C-M-z" . crux-indent-defun)
         ("M-o" . crux-smart-open-line)
         ("C-^" . crux-top-join-line)
         ("s-j" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))


(provide '+core)
;;; +core.el ends here.
