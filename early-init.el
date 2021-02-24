;;; early-init.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 - 2021 Andrei Shkarin
;; Author: Andrei Shkarin <andrei.shkarin@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file performs the early initialization that was introduced
;; in Emacs 27 and is loaded before the `init.el' file is loaded
;; and the user interface is initialized. It contains a number of
;; settings to optimise loading time.

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

(defvar better-gc-cons-threshold 67108864
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; The garbage collection can easily double startup time, so it is
;; better to suppress it temporarily and reset it after to avoid
;; freezes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold better-gc-cons-threshold
          gc-cons-percentage 0.1)))

;; As the garbage collection can slow down expensive commands or
;; completion frameworks, it is better to avoid this when
;; the minibuffer is active by raising the threshold.
(add-hook 'minibuffer-setup-hook
  (lambda ()
    (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
  (lambda ()
    (run-at-time 1 nil (lambda ()
                         (setq gc-cons-threshold better-gc-cons-threshold)))))

;; All Emacs file access and filename conversion primitives check
;; the filename for ~regex~ to use the correct ~handler~, which
;; are stored in the variable =file-name-handler-alist=.
;; This check is not required during the initialisation and can be
;; temporarily disabled.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-original)) t)

;; By default Emacs 27 calls `package-initialize' before
;; the `init.el' file is loaded, but after the `early-init.el'.
;; This function is supposed to find all installed packages and
;; activate them (depending on the optional argument) by
;; executing their `<pkg>-autoloads.el' files.
;;
;; As this configuration does not use the built-in package manager
;; this call is disabled.
(setq package-enable-at-startup nil)

;; In Emacs 27 a new `package-quickstart' feature has been
;; introduced. When it is non-nil, `package.el' precomputes
;; a big autoload file so that activation of packages can be done
;; much faster affecting startup time.
;;
;; As this configuration does not use the built-in package manager
;; this feature is disabled.
(setq package-quickstart nil)

;; By default Emacs tries to to keep the number of rows and
;; columns in the text area of the frame unchanged, which has
;; undesirable consequences:
;; - Increased start-up time when changing the font, as resizing
;;   the Emacs frame can be very expensive.
;; - If the font size is changed in full screen mode, the frame
;;   size will also be changed, so some of it might be out of
;;   the screen.
;;
;; For this reason, the implied frame resizing is disabled.
(setq frame-inhibit-implied-resize t)

;; Remove visual clutter.
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)

;;; early-init.el ends here
