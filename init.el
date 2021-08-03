;;; init.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file performs Emacs initialization and configuration.

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

(when (version< emacs-version "26.1")
  (error "Emacs 26.1 or higher is required."))


;; Collect some system information
(defconst is-mac     (eq system-type 'darwin))
(defconst is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst emacs-user (getenv (if is-windows "USERNAME" "USER")))

;;; Define directory structure

(defvar user-emacs-config-directory (file-name-directory load-file-name)
  "The root directory of the configuration.")

(defvar user-emacs-etc-directory (expand-file-name "etc" user-emacs-directory)
  "The directory where packages place configuration files.")

(defvar user-emacs-var-directory (expand-file-name "var" user-emacs-directory)
  "The directory where packages place persistent data files.")

(defvar user-emacs-cache-directory (expand-file-name "cache" user-emacs-var-directory)
  "The directory where packages place cached data.")

(let ((dirs (list user-emacs-etc-directory
                  user-emacs-var-directory
                  user-emacs-cache-directory)))
  (dolist (dir dirs)
    (unless (file-exists-p dir)
      (make-directory dir t))))

(when (version< emacs-version "27.1")
  (load-file (expand-file-name "early-init.el" user-emacs-config-directory)))


;;; General configuration

(setq load-prefer-newer t)

;; Disable the warning "X and Y are the same file".
(setq find-file-suppress-same-file-warnings t)

;; Lockfiles
(setq create-lockfiles nil)

;; Backup files
(let ((backup-directory (expand-file-name "autosave" user-emacs-cache-directory)))
  (setq make-backup-files            nil
        backup-by-copying            t   ; don't clobber symlinks
        version-control              t   ; number each-backup file
        delete-old-versions          t   ; clean-up after itself
        kept-old-versions            5
        kept-new-versions            5
        backup-directory-alist       (list (cons "." backup-directory))
        tramp-backup-directory-alist backup-directory-alist))

;; Auto-save
(let ((tramp-file-name-regex "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'")
      (file-name-regex ".*"))
  (setq auto-save-default               t
        auto-save-include-big-deletions t
        auto-save-list-file-prefix      (expand-file-name "autosave" user-emacs-cache-directory)
        tramp-auto-save-directory       (expand-file-name "autosave" user-emacs-cache-directory)
        auto-save-file-name-transforms  (list (list tramp-file-name-regex
                                                    (expand-file-name "tramp-\\2" auto-save-list-file-prefix) t)
                                              (list file-name-regex
                                                    auto-save-list-file-prefix t))))

;; Custom configuration file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


;;; Native compilation

;; Native Compilation modifies the load machinery such that it can
;; load .eln files in addition to conventional .elc and .el files.
;;
;; When a .elc file is being loaded if a suitable .eln file is
;; found in one of the eln-cache directories this is loaded
;; instead. Otherwise the file is compiled asyncronously and its
;; definitions are swapped once finished with that.
;; [https://akrl.sdf.org/gccemacs.html]

(when (boundp 'comp-eln-load-path)
    (let ((eln-cache-dir (expand-file-name "eln" user-emacs-cache-directory)))
      (setcar comp-eln-load-path eln-cache-dir)))

(with-eval-after-load 'comp
  ;; Disable native-compilation for some packages by adding regexes
  ;; to `comp-deferred-compilation-deny-list'
  (mapc (apply-partially #'add-to-list 'comp-deferred-compilation-deny-list)
      (list "\\(?:[^z-a]*-autoloads\\.el$\\)")))


;;; Install and Configure the `straight.el' package manager

(setq straight-base-dir user-emacs-var-directory)

;; `straight.el' uses symlinks in the `build' directory.
;; As Emacs asks for a confirmation to follow a symlink by default,
;; it should be disabled.
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; By default `straight.el' detects at boot time packages to
;; recompile based on changes of files they depend on. The time
;; taken for this process can be saved by configuring it to detect
;; package modifications when they are changed in Emacs.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; `straight.el' allows a package to be installed from a git
;; repository using the git backend command, which can also be
;; configured. By default, a full repository clone is done via
;; https. As complete repository history is not required,
;; the depth is reduced to save network bandwidth and disk space.
(setq straight-vc-git-default-clone-depth 1)

;; As private repositories cannot be cloned over https,
;; configure git to use ssh by setting variable.
(setq straight-vc-git-default-protocol 'ssh)

;; Bootstrap
(defvar bootstrap-version)

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        straight-base-dir))
      (bootstrap-version 5))
  ;; Obtaind bootstrap file
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; Catch Emacs updates that have native compiled leftovers
  (unless (catch 'emacs-version-changed
        (load bootstrap-file nil 'nomessage))
    (when (boundp 'comp-eln-load-path)
      ;; remove leftovers, with confirmation just to be safe
      (when (yes-or-no-p (format "Delete '%s'?" (car comp-eln-load-path)))
    (delete-directory (expand-file-name (car comp-eln-load-path)) t))
      ;; and try again
      (load bootstrap-file nil 'nomessage))))


;;; Install and `use-package' macro

;; `use-package' is a macro providing a convenient way to install
;; and configure Emacs packages. It installs packages via
;; a connected package manager and helps reduce startup time by
;; autoloading packages instead of loading them on startup.

(straight-use-package 'use-package)

;; Configure `use-package' to use `straight-el' by default.
(setq straight-use-package-by-default t)


;;; Load Modules

;; Add directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-config-directory))
(require '+editor)
(require '+ui)
(require '+tools)
(require '+python)
(require '+golang)
(require '+org)

;;; init.el ends here
