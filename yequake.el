;;; yequake.el --- Drop-down Emacs frames, like Yakuake  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/yequake.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (a "0.1.0"))
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

(require 'a)

;;;; Customization

(defgroup yequake nil
  "Settings for `yequake'."
  :link '(url-link "http://example.com/yequake.el")
  :group 'convenience)

(defcustom yequake-frames nil
  "List of predefined framesets for Yakuake to display.
Each value should be an alist setting at least these keys:

`name': Frame name

`buffer-fns': List of functions and strings used to display buffers.  Each entry should be either a function, which returns a buffer to display or splits the window and does not return a buffer, or a string, which either names an existing buffer or is the path to a file to display.

`width': An integer pixel width, or a float fraction of the monitor width.
`height': An integer pixel height, or a float fraction of the monitor height.

These keys may optionally be set:

`alpha': Float opacity for the new frame."
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
            (cons :tag "Alpha" (const alpha) float)
            (cons :tag "Buffer Functions" (const buffer-fns)
                  (repeat (choice (string :tag "Buffer or file name")
                                  (function :tag "Function that returns a buffer or splits the window"))))))))

(defcustom yequake-resize-delay 0.025
  "Time in seconds to wait between positioning a frame and resizing it.
I have no idea why this is necessary, but on my system it is.
Without it, setting the frame width undoes the just-changed
height, and vice versa."
  :type 'float)

;;;; Functions

;;;;; Commands

;;;###autoload
(defun yequake-toggle (frame-name)
  "Toggle the Yequake frame named FRAME-NAME."
  (interactive (list (completing-read "Frame: " yequake-frames)))
  (when-let* ((frame (a-get yequake-frames frame-name)))
    (yequake--toggle-frame frame)))

;;;;; Support

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

(defun yequake--toggle-frame (frame)
  "If FRAME is visible, delete it; otherwise, display it anew."
  (if-let* ((name (a-get frame 'name))
            (visible-frame (a-get (make-frame-names-alist) name)))
      ;; Frame is visible: hide it.
      (delete-frame visible-frame)
    ;; Show frame
    (-let* (((&alist '_x '_y 'width 'height 'buffer-fns 'alpha) frame)
            ((_monitor-x monitor-y monitor-width monitor-height) (mapcar #'floor (alist-get 'geometry (frame-monitor-attributes))))
            (frame-width (cl-typecase width
                           (integer width)
                           (float (floor (* monitor-width width)))))
            (frame-height (cl-typecase height
                            (integer height)
                            (float (floor (* monitor-height height)))))
            (frame-x (floor (/ (- monitor-width frame-width) 2)))
            (new-frame (make-frame (a-list 'name name
                                           'alpha alpha))))
      (set-frame-position new-frame frame-x monitor-y)
      ;; I wish this `sleep-for' weren't necessary, but I can't find a way around it, otherwise
      ;; setting the frame width undoes the setting of the frame height, or vice versa.
      (sleep-for yequake-resize-delay)
      (set-frame-height new-frame frame-height nil 'pixel)
      (set-frame-width new-frame frame-width nil 'pixel)
      (select-frame new-frame)
      (delete-other-windows)
      (yequake--show-buffers buffer-fns)
      (select-frame-set-input-focus new-frame)
      new-frame)))

;;;; Footer

(provide 'yequake)

;;; yequake.el ends here
