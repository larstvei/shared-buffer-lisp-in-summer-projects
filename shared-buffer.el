;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT
;; ABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
;; License for more details.

(require 'cl-lib)

(defstruct sb-package
  "This struct defines the format for packages sent to and
  received from the server."
  (start 1) (bytes 0) text for-new-client (cursor sb-point) id color)

(defstruct sb-client
  "This struct contains information about the other clients connected to the
same shared buffer. The cursor is just an overlay object, and the color is
  the color of the overlay. The timer ensures that an overlay will (if not
  moved) be deleted after ten seconds."
  cursor color timer)

(defcustom sb-port 3705
  "Shared-buffer uses port 3705 by default."
  :group 'shared-buffer
  :type 'integer)

(defvar sb-key ""
  "A buffer-local string containing the key to the associated
  shared-bufer-session.")

(defvar sb-server nil
  "A buffer-local variable pointing to the server a shared buffer is
  connected to.")

(defvar sb-clients (make-hash-table)
  "A hash-table containing the other clients connected to the same
  shared-buffer.")

(defvar sb-dont-send nil
  "Indecates that the next package should not be sent, if set to a non-nil
  value.")

(defvar sb-point 1
  "A variable containing the current cursor position.")

(defun sb-connect-to-server (host buffer)
  "This function opens a connection to a shared-buffer server, by starting a
shared-buffer client."
  (switch-to-buffer buffer)
  (make-local-variable 'sb-key)
  (make-local-variable 'sb-server)
  (make-local-variable 'sb-clients)
  (make-local-variable 'sb-dont-send)
  (make-local-variable 'sb-point)
  (setq sb-key (read-from-minibuffer "key: "))
  (setq sb-server
        (make-network-process
         :name "sb-client" :buffer buffer
         :filter 'sb-client-filter :sentinel 'sb-client-sentinel
         :family 'ipv4 :host host :service sb-port))
  (add-hook 'after-change-functions 'sb-send-update nil 'local)
  (add-hook 'post-command-hook 'sb-send-cursor-update nil 'local)
  (equal 'open (process-status sb-server)))

(defun sb-connect-to-shared-buffer (host &optional buffer)
  "This interactive function is used to connect to an already existing
shared-buffer-session."
  (interactive "sHost: ")
  (if (sb-connect-to-server
       host (or buffer (generate-new-buffer "*shared-buffer*")))
      (process-send-string sb-server (concat "existing\n" sb-key "\n"))
    (message "Could not connect.")
    (kill-buffer))
  (set-process-filter sb-server 'sb-client-new-connection-filter))

(defun sb-share-this-buffer (host &optional buffer)
  "This interactive function is used to initiate a shared-buffer-session."
  (interactive "sHost: ")
  (if (sb-connect-to-server
       host (or buffer (current-buffer)))
      (process-send-string sb-server (concat "new\n" sb-key "\n"))
    (message "Could not connect.")))

(defun sb-send-cursor-update ()
  (unless sb-dont-send
    (sb-send-update 1 1 0))
  (setq sb-dont-send nil))

(defun sb-send-update (start end bytes)
  "Sends an update to the server. This function is locally added
to the 'after-change-functions hook for shared buffers."
  (when this-command
    (setq sb-point (point)))
  (sb-send-package start bytes (buffer-substring-no-properties start end))
  (setq sb-dont-send t))

(defun sb-send-package (start bytes string)
  "Sends a package to the server. TCP has a maximum package size of 4KB, so
we have to make sure not to send packages this large. This is resolved by
  splitting strings larger than 2KB in 2KB chunks."
  (loop for text in (sb-string-chunks (expt 2 11) string) do
        (process-send-string
         sb-server
         (concat
          (prin1-to-string
           (make-sb-package
            :start start :bytes bytes
            :text (split-string text "\\(\r\n\\|[\n\r]\\)"))) "\n"))
        (setq start (+ start (expt 2 11)))))

(defun sb-string-chunks (max-len str)
  "Returns a list of strings, where max-len is the maximum length of each
string."
  (if (< (length str) max-len)
      (list str)
    (cons (substring str 0 max-len)
          (sb-string-chunks max-len (substring str max-len)))))

(defun sb-close ()
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
  (delete-overlay (sb-client-cursor client))
  (goto-char cursor-point)
  (if (eolp)
      (sb-insert-cursor-at-eol client cursor-point)
    (sb-insert-cursor-inline client cursor-point))
  (sb-reset-timer client))

(defun sb-reset-timer (client)
  "A cursor will be removed after being idle for 10 seconds. If the cursor
is updated within this time frame the timer must be reset."
  (when (timerp (sb-client-timer client))
    (cancel-timer (sb-client-timer client))
    (setf (sb-client-timer client)
          (run-at-time "10 sec" nil (lambda (o) (delete-overlay o))
                       (sb-client-cursor client)))))

(defun sb-update-buffer (package buffer)
  "Makes changes to the shared buffer specified by the package."
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
          (client (gethash (sb-package-id package) sb-clients)))
      (set-buffer buffer)
      (setq inhibit-modification-hooks t)
      (unless (sb-package-for-new-client package)
        (goto-char (sb-package-start package))
        (delete-char (sb-package-bytes package))
        (sb-insert (sb-package-text package))
        (sb-move-cursor client (sb-package-cursor package)))
      (setq inhibit-modification-hooks nil)
      (set-buffer old-curr-buf))))

(defun sb-client-new-connection-filter (process msg)
  "This filter is used when a connection is initiated. During
  most of a shared buffer session, sb-client-filter will be used."
  (save-excursion
    (let ((old-curr-buf (current-buffer))
          (package (read msg)))
      (set-buffer (process-buffer process))
      (setq inhibit-modification-hooks t)
      (sb-insert (sb-package-text package))
      (setq inhibit-modification-hooks nil)
      (set-buffer old-curr-buf)))
  (set-process-filter process 'sb-client-filter))

(defun message-from-server (process msg)
  "The sever does not send messages with the sb-package format. These
messages are handled in this function."
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (set-buffer (process-buffer process))
      (cond ((not (stringp msg)) (print msg))
            ((string= msg "send-everything")
             (sb-send-package 1 0 (buffer-substring-no-properties
                                   (point-min) (point-max))))
            ((string-match "The key " msg)
             (kill-buffer (process-buffer process))
             (print msg))
            (t (print msg)))
      (set-buffer old-buffer))))

(defun sb-client-filter (process msg)
  "The filter function handles all messages from the server."
  (if (not (string-match "\\[cl-struct-sb-package" msg))
      (message-from-server process msg)
    (sb-update-buffer (read msg) (process-buffer process))))

(defun sb-client-sentinel (process msg)
  (message msg))
