;;; +ui.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Setup UI.

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


;; Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (ielm-mode . rainbow-delimiters-mode))


;; Visualization of tabs, spaces, newline, etc.
(use-package whitespace
  :straight nil
  :hook ((prog-mode   . whitespace-mode)
         (text-mode   . whitespace-mode)
         (before-save . whitespace-cleanup))
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


;; Highlighting of the current line
(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode +1))


;; Highlighting of the keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))


;; Sets background color of strings that matches color names
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))


;; Theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))


;; Fullscreen

;; maximize the initial frame automatically
;(set-frame-parameter nil 'fullscreen 'fullboth)

(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (progn
        (set-frame-parameter nil 'fullscreen 'fullheight)
        (menu-bar-mode t))
    (progn
      (set-frame-parameter nil 'fullscreen 'fullboth)
      (menu-bar-mode -1))))

(global-set-key (kbd "<f12>") 'toggle-fullscreen)


(provide '+ui)
;;; +ui.el ends here.
