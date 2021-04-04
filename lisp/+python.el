;;; +python.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Install and configure packages for python.

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


(use-package python
  :ensure t
  :config
  (setq python-shell-interpreter "python3"))


(use-package lsp-python-ms
  :ensure t
  :after (lsp-mode python)
  :config
  (setq lsp-python-ms-python-executable-cmd python-shell-interpreter))


(use-package pyvenv
  :ensure t
  :after python
  :hook (python-mode . pyenv-mode)
  :config
  (setq pyvenv-default-virtual-env-name "env")
  (setq pyvenv-mode-line-indicator
    '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "]"))))


(use-package python-pytest
  :ensure t)


(use-package dap-python
  :straight nil
  :after (dap-mode python)
  :config
  (setq dap-python-executable python-shell-interpreter))


(provide '+python)
;;; +python.el ends here.
