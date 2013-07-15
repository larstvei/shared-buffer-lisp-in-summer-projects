;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

(ql:quickload "usocket")

(defconstant +port+ 3705
  "Shared-buffer uses port 3705.")

(defparameter *client-groups* nil
  "Hash table containing lists of clients working on the same buffer.")

(defun handler (stream)
  "When a connection to a client is established this function is run. It
  will read from the stream as long as the connection is open, and redirect
  messages to all clients that has provided the same key."
  (let ((key (read-line stream)))
    (setf (gethash key *client-groups*)
          (cons stream (gethash key *client-groups*)))
    ;; Reading the stream until EOF.
    (loop for message = (read-line stream nil)
       while message do
         (print message)
         (force-output)
         (send-update
          message (remove stream (gethash key *client-groups*))))
    ;; After reaching EOF we remove the client from the client group.
    (setf (gethash key *client-groups*)
          (remove stream (gethash key *client-groups*)))))

(defun send-update (message client-group)
  "Simply sends a message recived from a client to all clients sharing a
buffer."
  (loop for stream in client-group do
       (write-string message stream)
       (force-output stream)))

(defun shared-buffer-server (host)
  "Starts a server for the emacs extension shared-buffer."
  (setf *client-groups* (make-hash-table :test #'equal))
  (usocket:socket-server host +port+
                         #'handler nil
                         :in-new-thread t
                         :reuse-address t
                         :multi-threading t))
