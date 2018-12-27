;;; yequake.el --- Drop-down frames, like Yakuake  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/yequake
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (dash "2.14.1"))
;; Keywords: convenience, window-system, frames

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides configurable, drop-down Emacs frames, similar to drop-down terminal windows
;; programs, like Yakuake.  Each frame can be customized to display certain buffers in a certain
;; way, at the desired height, width, and opacity.  The idea is to call the `yequake-toggle' command
;; from outside Emacs, using emacsclient, by binding a shell command to a global keyboard shortcut
;; in the desktop environment.  Then, with a single keypress, the desired Emacs frame can be toggled
;; on and off, showing the desired buffers.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + a

;; Then put this file in your load-path, and put this in your init file:

;; (require 'yequake)

;;;; Usage

;; Run one of these commands:

;; `yequake-toggle': Toggle a configured Yequake frame.

;; This is mainly intended to be called from outside Emacs.  So:

;; 1.  Start an Emacs daemon.
;; 2.  Run in a shell:  emacsclient -n -e "(yekuake-toggle "FRAME-NAME")"

;; If you bind that shell command to a global keyboard shortcut, you can easily toggle the frame on
;; and off.

;;;; Tips

;; + You can customize settings in the `yequake' group.

;;;; Credits

;; Inspired by Benjamin Slade's `equake' package: <https://gitlab.com/emacsomancer/equake>

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'subr-x)

(require 'dash)

;;;; Compatibility

;; For Emacs <26

(unless (fboundp 'if-let*)
  (defalias 'if-let* 'if-let))
(unless (fboundp 'when-let*)
  (defalias 'when-let* 'when-let))

;;;; Customization

(defgroup yequake nil
  "Drop-down Emacs frames, like Yakuake."
  :link '(url-link "http://github.com/alphapapa/yequake")
  :group 'convenience)

(defcustom yequake-frames nil
  "List of predefined framesets for Yakuake to display.
Each value should be an alist setting at least these keys:

`name': Frame name.

`buffer-fns': List of functions and strings used to display
buffers.  Each entry should be either a function, which returns a
buffer to display or splits the window and does not return a
buffer, or a string, which either names an existing buffer or is
the path to a file to display.

`width': An integer pixel width, or a float fraction of the
monitor width.

`height': An integer pixel height, or a float fraction of the
monitor height.

`left': An integer pixel distance from the workspace left edge,
or a floating point ratio of the workspace width.  See Info
node `(elisp)Position Parameters'.

`top': An integer pixel distance from the workspace top edge, or
a floating point ratio of the workspace height.  See Info
node `(elisp)Position Parameters'.

`frame-parameters': Alist of parameters passed to `make-frame',
e.g. `alpha' to set the frame opacity.

See Info node `(elisp)Frame Parameters'."
  ;; NOTE: The value-type could also be done like this, but it allows other key values, which looks
  ;; a little confusing in the customization UI.  This doesn't allow other values, but it does show
  ;; both the tag and const name, which is redundant and a bit confusing.

  ;; (alist :options (((const :tag "Name" name) string)
  ;;                  ((const :tag "Width" width) number)
  ;;                  ((const :tag "Height" height) number)
  ;;                  ((const :tag "Alpha" alpha) number)
  ;;                  ((const :tag "Buffer functions" buffer-fns)
  ;;                   (repeat (choice (string :tag "Buffer or file name")
  ;;                                   (function-item :tag "Function that returns a buffer or splits the window"))))))
  :type '(alist
          :key-type (string :tag "Frame name")
          :value-type
          (repeat
           (choice
            (cons :tag "Name" (const name) string)
            (cons :tag "Width" (const width) number)
            (cons :tag "Height" (const height) number)
            (cons :tag "Left" (const left) number)
            (cons :tag "Top" (const top) number)
            (cons :tag "Buffer Functions" (const buffer-fns)
                  (repeat (choice (string :tag "Buffer or file name")
                                  (function :tag "Function that returns a buffer or splits the window"))))
            (cons :tag "Frame parameters" :format "%t: %h"
                  :doc "See info(elisp) Frame Parameters."
                  (const frame-parameters)
                  (alist :options (((const :tag "Opacity" alpha) float))))))))

;;;; Variables

(defvar yequake-focused nil
  ;; This seems to be the only way to find out whether an Emacs frame has input focus in the window
  ;; system.  See
  ;; <https://emacs.stackexchange.com/questions/24597/determine-if-any-frame-has-input-focus> and
  ;; <https://lists.gnu.org/archive/html/help-gnu-emacs/2015-11/msg00160.html>.
  "Tracks whether Emacs has focus.")

;;;; Functions

;;;;; Commands

;;;###autoload
(defun yequake-toggle (name)
  "Toggle the Yequake frame named NAME."
  (interactive (list (completing-read "Frame: " yequake-frames)))
  (if-let* ((frame (alist-get name yequake-frames nil nil #'string=)))
      (yequake--toggle-frame frame)
    (user-error "No Yequake frame named: %s" name)))

;;;;; Support

(defun yequake--toggle-frame (frame)
  "If FRAME exists but is unfocused, raise and focus it; if focused, delete it; otherwise, display it anew."
  (if-let* ((name (alist-get 'name frame))
            (visible-frame (alist-get name (make-frame-names-alist) nil nil #'string=)))
      (if (and yequake-focused (equal visible-frame (selected-frame)))
          ;; Frame is visible and focused: delete it.
          (delete-frame visible-frame)
        ;; Frame is visible but not focused: raise and focus it.
        (select-frame-set-input-focus visible-frame)
        (setq yequake-focused t))
    ;; Frame doesn't exist: make it.
    (-let* (((&alist '_x '_y 'width 'height 'left 'top 'buffer-fns 'alpha 'frame-parameters) frame)
            ((_monitor-x _monitor-y monitor-width monitor-height) (mapcar #'floor (alist-get 'geometry (frame-monitor-attributes))))
            (frame-width (cl-typecase width
                           (integer width)
                           (float (floor (* monitor-width width)))))
            (frame-height (cl-typecase height
                            (integer height)
                            (float (floor (* monitor-height height)))))
            (frame-x (or left
                         (floor (/ (- monitor-width frame-width)
                                   2))))
            (frame-y (or top
                         (floor (/ (- monitor-height frame-height)
                                   2))))
            (new-frame (make-frame (append frame-parameters
                                           (list (cons 'name name)
                                                 (cons 'alpha alpha)
                                                 (cons 'left frame-x)
                                                 (cons 'top frame-y)
                                                 (cons 'width (cons 'text-pixels frame-width))
                                                 (cons 'height (cons 'text-pixels frame-height))
                                                 (cons 'user-position t))))))
      (select-frame new-frame)
      (delete-other-windows)
      (yequake--show-buffers buffer-fns)
      (select-frame-set-input-focus new-frame)
      (setq yequake-focused t)
      (add-hook 'focus-out-hook #'yequake--focus-out)
      new-frame)))

(defun yequake--show-buffers (buffer-fns)
  "Show buffers returned by each function in BUFFER-FNS."
  (cl-flet ((act (it) (cl-typecase it
                        (string (or (get-buffer it)
                                    (find-buffer-visiting it)
                                    (find-file-noselect it)))
                        (function (funcall it)))))
    (let ((split-width-threshold 0)
          (split-height-threshold 0))
      ;; Switch to first buffer, pop to the rest.
      (switch-to-buffer (act (car buffer-fns)))
      (dolist (fn (cdr buffer-fns))
        (when-let* ((ret (act fn)))
          (cl-typecase ret
            (window (select-window ret))
            (buffer (display-buffer-same-window ret nil))))))))

(defun yequake--focus-out ()
  "Set `yequake-focused' to nil.
To be added to `focus-out-hook'."
  (setq yequake-focused nil))

;;;; Footer

(provide 'yequake)

;;; yequake.el ends here
