;;; +editor.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Setup packages configuring editor.

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
;; Keybindings

;; Completion on text around point
(global-set-key (kbd "C-<tab>") 'complete-symbol)

;; Windows
(define-key global-map (kbd "C-c w") (make-sparse-keymap))
(global-set-key (kbd "C-c w o") 'other-window)
(global-set-key (kbd "C-c w k") 'delete-window)
(global-set-key (kbd "C-c w s v") 'split-window-vertically)
(global-set-key (kbd "C-c w s h") 'split-window-horizontally)

;; Regions
(define-key global-map (kbd "C-c r") (make-sparse-keymap))
(global-set-key (kbd "C-c r TAB") 'indent-region)
(global-set-key (kbd "C-c r j") 'json-pretty-print)
(global-set-key (kbd "C-c r c") 'comment-region)
(global-set-key (kbd "C-c r u") 'uncomment-region)


;;
;; Packages


;; Preserves location in files when saving
(use-package saveplace
  :straight nil
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-cache-directory))
  (setq-default save-place t))


;; Loads previous minibuffer histories from `savehist-file'
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-emacs-cache-directory))
  :init
  (savehist-mode))


;; Sets unique names for buffer dependent on file name
(use-package uniquify
  :straight nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; ignore special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))


;; Auto-save buffers, when certain events happen
(use-package super-save
  :ensure t
  :init
  (super-save-mode))


;; Undo tree
(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode))


;; Items selection from a list
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; This fixes a problem of hidden entry line
  (defun vertico--display-candidates (lines)
  "Update candidates overlay `vertico--candidates-ov' with LINES."
    (move-overlay vertico--candidates-ov (point-max) (point-max))
    (overlay-put vertico--candidates-ov 'after-string
                 (apply #'concat #(" " 0 1 (cursor t)) (and lines "\n") lines))
    (vertico--resize-window (length lines))))

(use-package orderless
  :after vertico
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; In-buffer completion mechanism
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
  :init
  (global-company-mode))

;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-cache-directory)))


;; Spell checking (on-the-fly)
(use-package flyspell
  :hook ((text-mode      . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")))


;; Navigation for imenu tags across all buffers that
;; satisfy a filtering criteria
(use-package imenu-anywhere
  :ensure t)


;; Displays the key bindings following currently
;; entered incomplete command
(use-package which-key
  :ensure t
  :init
  (which-key-mode))


;; Navigation from window to window
(use-package windmove
  :straight nil
  :init
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))


;; Rotations
(use-package rotate
  :ensure t
  :bind (("C-c w r w" . rotate-window)
         ("C-c w r l" . rotate-layout)))


;; Workspaces
(use-package perspective
  :bind (("C-x C-b" . persp-buffer-menu)
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode))


(provide '+editor)
;;; +editor.el ends here.
