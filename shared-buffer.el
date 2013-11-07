;;; shared-buffer.el --- Collaberative editing in Emacs.

;; Copyright (C) 2013 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

;; Shared buffer is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Shared buffer is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Shared buffer.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl)

(defstruct sb-package
  "This struct defines the format for packages sent to and
  received from the server."
  (start 1) (bytes 0) (max sb-point-max) text for-new-client
  (cursor sb-point) region-start region-end id color)

(defstruct sb-client
  "This struct contains information about the other clients connected to the
same shared buffer. The cursor is just an overlay object, and the color is
  the color of the overlay. The timer ensures that an overlay will (if not
  moved) be deleted after ten seconds."
  region cursor color timer)

(defstruct sb-message
  "Each message received is prefixed with a length. The message will not be
evaluated before all bytes are received."
  (bytes-left 0) (message ""))

(defcustom sb-port 3705
  "Shared-buffer uses port 3705 by default."
  :group 'shared-buffer
  :type 'integer)

(defvar sb-key ""
  "A buffer-local string containing the key to the associated
  shared-buffer-session.")

(defvar sb-server nil
  "A buffer-local variable pointing to the server a shared buffer is
  connected to.")

(defvar sb-clients (make-hash-table)
  "A hash-table containing the other clients connected to the same
  shared-buffer.")

(defvar sb-dont-send nil
  "Indicates that the next package should not be sent, if set to a non-nil
  value.")

(defvar sb-point-max 1
  "A variable containing what (point-max) returned before a change was
  made.")

(defvar sb-point 1
  "A variable containing the current cursor position.")

(defvar sb-msg (make-sb-message)
  "A message from the server is temporarily stored in sb-msg. A message
  larger than 1KB will be read in 1KB bulks.")

(defvar sb-new-client t
  "When a client has just connected it has to receive the shared buffer's
  content.")

(defun sb-connect-to-server (host buffer)
  "This function opens a connection to a shared-buffer server, by starting a
shared-buffer client."
  (switch-to-buffer buffer)
  (make-local-variable 'sb-key)
  (make-local-variable 'sb-server)
  (make-local-variable 'sb-clients)
  (make-local-variable 'sb-dont-send)
  (make-local-variable 'sb-new-client)
  (make-local-variable 'sb-point-max)
  (make-local-variable 'sb-point)
  (setq sb-key (read-from-minibuffer "key: "))
  (setq sb-server
        (make-network-process
         :name "sb-client" :buffer buffer
         :filter 'sb-client-filter :sentinel 'sb-client-sentinel
         :family 'ipv4 :host host :service sb-port))
  (add-hook 'after-change-functions 'sb-send-update nil 'local)
  (add-hook 'post-command-hook 'sb-send-cursor-update nil 'local)
  (add-hook 'before-change-functions 'sb-update-point-max nil 'local)
  (put 'sb-send-update 'permanent-local-hook t)
  (put 'sb-send-cursor-update 'permanent-local-hook t)
  (put 'sb-update-point-max 'permanent-local-hook t)
  (equal 'open (process-status sb-server)))

(defun sb-connect-to-shared-buffer (host &optional buffer)
  "This interactive function is used to connect to an already existing
shared-buffer-session."
  (interactive "sHost: ")
  (if (sb-connect-to-server
       host (or buffer (generate-new-buffer "*shared-buffer*")))
      (process-send-string
       sb-server (encode-coding-string
                  (concat "existing\n" sb-key "\n") 'utf-8))
    (message "Could not connect.")
    (sb-disconnect)
    (kill-buffer)))

(defun sb-share-this-buffer (host &optional buffer)
  "This interactive function is used to initiate a shared-buffer-session."
  (interactive "sHost: ")
  (if (sb-connect-to-server
       host (or buffer (current-buffer)))
      (process-send-string
       sb-server (encode-coding-string (concat "new\n" sb-key "\n") 'utf-8))
    (message "Could not connect."))
  (setq sb-new-client nil))

(defun sb-send-cursor-update ()
  "This function is run after a command has been executed. It sends a
package to the server containing cursor location."
  (unless sb-dont-send
    (sb-send-update 1 1 0))
  (setq sb-dont-send nil))

(defun sb-update-point-max (start end)
  (setq sb-point-max (point-max)))

(defun sb-send-update (start end bytes)
  "Sends an update to the server. This function is locally added
to the 'after-change-functions hook for shared buffers."
  (when this-command
    (setq sb-point (point)))
  (sb-send-package start bytes (buffer-substring-no-properties start end))
  (setq sb-dont-send t))

(defun sb-send-package (start bytes string &optional for-new-client)
  "Sends a package to the server."
  (process-send-string
   sb-server
   (encode-coding-string
    (concat
     (prin1-to-string
      (make-sb-package
       :start start :bytes bytes
       :text (split-string string "\\(\r\n\\|[\n\r]\\)")
       :for-new-client for-new-client
       :region-start (when (region-active-p) (region-beginning))
       :region-end (when (region-active-p) (region-end)))) "\n") 'utf-8)))

(defun sb-string-chunks (max-len str)
  "Returns a list of strings, where max-len is the maximum length of each
string."
  (if (< (length str) max-len)
      (list str)
    (cons (substring str 0 max-len)
          (sb-string-chunks max-len (substring str max-len)))))

(defun sb-disconnect ()
  "Closes the connecton to the server."
  (interactive)
  (remove-hook 'post-command-hook 'sb-send-cursor-update 'local)
  (remove-hook 'after-change-functions 'sb-send-update 'local)
  (delete-process sb-server)
  (setq sb-server nil))

(defun sb-insert (strings)
  "Each string in strings represents a line. Messages containing only one
string does not contain a line break, but all others naturally does."
  (insert (car strings))
  (mapc (lambda (str)
          (newline) (insert str)) (cdr strings)))

(defun sb-insert-cursor-at-eol (client cursor-point)
  "Inserts a cursor-like overlay at the end of a line."
  (setf (sb-client-cursor client)
        (make-overlay cursor-point cursor-point nil nil nil))
  (overlay-put (sb-client-cursor client) 'after-string
               (propertize " " 'face (cons 'background-color
                                           (sb-client-color client)))))

(defun sb-insert-cursor-inline (client cursor-point)
  "Inserts a cursor-like overlay."
  (setf (sb-client-cursor client)
        (make-overlay cursor-point (1+ cursor-point) nil nil nil))
  (overlay-put (sb-client-cursor client) 'face
               (cons 'background-color (sb-client-color client))))

(defun sb-move-cursor (client cursor-point)
  "Moves the a clients cursor. This function is heavily inspired by Magnar
Sveen's multible-cursors.el."
  (sb-delete-overlays client)
  (unless (> cursor-point (point-max))
    (goto-char cursor-point)
    (if (eolp)
        (sb-insert-cursor-at-eol client cursor-point)
      (sb-insert-cursor-inline client cursor-point)))
  (sb-reset-timer client))

(defun sb-update-region (client region-start region-end)
  "If a clients' region is active both region-start and region-end will be
integers. Then we simply add an overlay imitating a region."
  (when (and region-start region-end)
    (let ((overlay (make-overlay region-start region-end nil nil t)))
      (overlay-put overlay 'face '(:inherit region))
      (setf (sb-client-region client) overlay))))

(defun sb-reset-timer (client)
  "A cursor will be removed after being idle for 10 seconds. If the cursor
is updated within this time frame the timer must be reset."
  (when (timerp (sb-client-timer client))
    (cancel-timer (sb-client-timer client))
    (setf (sb-client-timer client)
          (run-at-time "10 sec" nil 'sb-delete-overlays client))))

(defun sb-delete-overlays (client)
  "Removes a clients cursor, and region if it's active."
  (when (sb-client-region client)
    (delete-overlay (sb-client-region client)))
  (delete-overlay (sb-client-cursor client)))

(defun sb-calculate-point (point max)
  "Given an absolute point from a shared buffer, we calculate where that
  point should be places in this buffer."
  (if (< (point) point) (+ point (- (point-max) max)) point))

(defun sb-update-buffer (package buffer)
  "Makes changes to the shared buffer specified by the package."
  (setq inhibit-modification-hooks t)
  (unless (gethash (sb-package-id package) sb-clients nil)
    ;; A new client has connected to the shared buffer.
    (puthash
     (sb-package-id package)
     (make-sb-client
      :cursor (make-overlay 1 1 nil nil nil)
      :color (sb-package-color package)
      :timer (run-at-time "0 sec" nil (lambda () 'dummy))) sb-clients))
  (save-excursion
    (let ((old-curr-buf (current-buffer))
          (client (gethash (sb-package-id package) sb-clients))
          (auto-fill auto-fill-function))
      (set-buffer buffer)
      (setq auto-fill-function nil)
      (unless (and (not sb-new-client)
                   (sb-package-for-new-client package))
        (goto-char
         (sb-calculate-point
          (sb-package-start package) (sb-package-max package)))
        (delete-char (sb-package-bytes package))
        (sb-insert (sb-package-text package))
        (sb-move-cursor client (sb-package-cursor package))
        (sb-update-region client (sb-package-region-start package)
                          (sb-package-region-end package)))
      (unless (sb-package-for-new-client package)
        (setq sb-new-client nil))
      (setq auto-fill-function auto-fill)
      (setq inhibit-modification-hooks nil)
      (set-buffer old-curr-buf))))

(defun sb-message-from-server (process msg)
  "The sever does not send messages with the sb-package format. These
messages are handled in this function."
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (when (buffer-live-p (process-buffer process))
        (set-buffer (process-buffer process)))
      (cond ((not (stringp msg)) (prin1 msg))
            ((string-match "The key " msg)
             (sb-disconnect) (kill-buffer)
             (message msg))
            ((string-match "send-everything" msg)
             (sb-send-package 1 0 (buffer-substring-no-properties
                                   (point-min) (point-max)) t))
            (t (prin1 msg)))
      (when (buffer-live-p old-buffer) (set-buffer old-buffer)))))

(defun sb-handle-received-string (process string)
  "Handles an incoming string. Empty strings are ignored, structs
are read and processed. Other messages are probably a message from the
  server and is treated in sb-message-from-server."
  (cond ((string= string "") 'ignore)
        ((string-match "\\[cl-struct-sb-package" string)
         (let ((package (read string)))
           (sb-update-buffer package (process-buffer process))))
        (t (sb-message-from-server process string))))

(defun sb-receive-new-message (message)
  "In this function we assume that there is a new message to read. All
messages are prefixed with the length of the message (or the number of
  bytes). The message is stored in the sb-message struct, and the part of
  the string that has still not been stored is returned."
  (let ((msg-start (string-match "[^[:digit:] ]" message)))
    (setf (sb-message-bytes-left sb-msg) (read message))
    (let* ((message-len (length message))
           (msg-part-len (+ msg-start (sb-message-bytes-left sb-msg)))
           (msg-end (if (< msg-part-len message-len)
                        msg-part-len message-len)))
      (setf (sb-message-message sb-msg)
            (substring message msg-start msg-end)
            (sb-message-bytes-left sb-msg)
            (- (sb-message-bytes-left sb-msg) (- msg-end msg-start)))
      (substring message msg-end))))

(defun sb-receive-message-part (message)
  "Here we assume that a message has already been partially received."
  (let* ((msg-end (and (< (sb-message-bytes-left sb-msg) (length message))
                       (sb-message-bytes-left sb-msg)))
         (msg-part (substring message 0 msg-end)))
    (setf (sb-message-message sb-msg)
          (concat (sb-message-message sb-msg) msg-part)
          (sb-message-bytes-left sb-msg)
          (- (sb-message-bytes-left sb-msg) (length msg-part)))
    (if msg-end (substring message msg-end) "")))

(defun sb-client-filter (process msg)
  "The filter function handles all messages from the server."
  (setq msg (decode-coding-string msg 'utf-8))
  (setq msg (if (zerop (sb-message-bytes-left sb-msg))
                (sb-receive-new-message msg)
              (sb-receive-message-part msg)))
  (when (zerop (sb-message-bytes-left sb-msg))
    (sb-handle-received-string process (sb-message-message sb-msg))
    (when (string-match "[^ ]" msg)
      (sb-client-filter process msg))))

(defun sb-client-sentinel (process msg)
  'ok)

;;; shared-buffer.el ends here.
