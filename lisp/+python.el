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


(use-package python-pytest
  :ensure t)


(use-package pyvenv
  :ensure t
  :after (python)
  :config
  (setq pyvenv-mode-line-indicator
    '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "]"))))


(use-package lsp-python-ms
  :ensure t
  :after (lsp-mode python))


(use-package dap-python
  :straight nil
  :after (dap-mode python)
  :config
  (setq dap-python-executable "python3")
  (dap-register-debug-template "Python :: Run file (buffer)"
                                (list :type "python"
                                      :cwd (lsp-workspace-root)
                                      :program nil
                                      :debugger 'debugpy
                                      :request "launch"
                                      :name "Python :: Run file (buffer)")))


(defun dap-python-install-packages ()
  "Install required Python packages for dap."
  (interactive)
  (if 'pyvenv-virtual-env
      (async-shell-command "pip install -U debugpy ptvsd")
    (message "No active virtualenv.")))


(defun pyvenv-autoload ()
  "Automatically activates pyvenv version if .venv directory exists."
  (if (equal major-mode 'python-mode)
      (f-traverse-upwards
        (lambda (path)
          (if (f-root? path)
              (progn
                (pyvenv-deactivate)
                (message "Deactivate python virtual environment"))
            (let ((venv-path (f-expand ".venv" path)))
                (if (f-exists? venv-path)
                    (progn
                      (message "Activating %s" venv-path)
                      (pyvenv-activate venv-path)
                      t))))) (projectile-project-root))
    (pyvenv-deactivate)))


;; Activate proper venv and LSP when python buffer opened
(add-hook 'python-mode-hook 'pyvenv-autoload)
(add-hook 'python-mode-hook 'lsp)

;; Update venv when buffer refocused
(add-hook 'buffer-list-update-hook 'pyvenv-autoload)

(provide '+python)
;;; +python.el ends here.
