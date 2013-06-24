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
