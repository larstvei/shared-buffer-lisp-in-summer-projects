;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

(ql:quickload "usocket")

(defparameter *client-groups* (make-hash-table)
  "Hash table containing lists of clients working on the same buffer.")

(defun handler (stream)
  "Function to handle new clients."
  (print (read-line stream)))

(defun shared-buffer-server (host port)
  "Starts a server for the emacs extension shared-buffer."
  (usocket:socket-server host port
                         #'handler nil
                         :in-new-thread t
                         :reuse-address t
                         :multi-threading t))
