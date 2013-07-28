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
  (start 1) (bytes 0) (text "") (for-new-client nil)
  (cursor sb-point) id)

(defconst sb-port 3705
  "Shared-buffer uses port 3705.")

(defvar sb-key ""
  "A buffer-local string containing the key to the associated
  shared-bufer-session.")

(defvar sb-server nil
  "A buffer-local variable pointing to the server a shared buffer is
  connected to.")

(defvar sb-cursors (make-hash-table)
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
  (make-local-variable 'sb-cursors)
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
    (message "Could not connect."))
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
  (let ((package
         (make-sb-package
          :start start :bytes bytes
          :text (split-string (buffer-substring-no-properties start end)
                              "\\(\r\n\\|[\n\r]\\)"))))
    (process-send-string sb-server
                         (concat (prin1-to-string package) "\n")))
  (setq sb-dont-send t))

(defun sb-close ()
  "Closes the connecton to the server."
  (interactive)
  (delete-process sb-server)
  (setq sb-server nil)
  (remove-hook 'after-change-functions 'sb-send-update 'local)
  (remove-hook 'post-command-hook 'sb-filter-updates 'local))

(defun sb-insert (strings)
  "Each string in strings represents a line. Messages containing only one
string does not contain a line break, but all others naturally does."
  (insert (car strings))
  (mapc (lambda (str)
          (newline) (insert str)) (cdr strings)))

(defun sb-insert-cursor-at-eol (id cursor-point)
  "Inserts a cursor-like overlay at the end of a line."
  (puthash id (make-overlay cursor-point cursor-point nil nil nil)
           sb-cursors)
  (overlay-put (gethash id sb-cursors) 'after-string
               (propertize " " 'face '(background-color . "green"))))

(defun sb-insert-cursor-inline (id cursor-point)
  "Inserts a cursor-like overlay."
  (puthash id (make-overlay cursor-point (1+ cursor-point) nil nil nil)
           sb-cursors)
  (overlay-put (gethash id sb-cursors) 'face
               '(t (background-color . "green"))))

(defun sb-move-cursor (id cursor-point)
  "Moves the a clients cursor. This function is heavily inspired by Magnar
Sveen's multible-cursors.el."
  (when (gethash id sb-cursors nil)
    (delete-overlay (gethash id sb-cursors nil)))
  (goto-char cursor-point)
  (if (eolp)
      (sb-insert-cursor-at-eol id cursor-point)
    (sb-insert-cursor-inline id cursor-point)))

(defun message-from-server (process msg)
  "The sever does not send messages with the sb-package format. These
messages are handled in this function."
  (cond ((string= msg "send-everything")
         (process-send-string
          sb-server
          (concat (prin1-to-string
                   (make-sb-package
                    :text (split-string (buffer-substring-no-properties
                                         (point-min) (point-max))
                                        "\\(\r\n\\|[\n\r]\\)")
                    :for-new-client t)) "\n")))
        ((string-match "The key " msg)
         (kill-buffer (process-buffer process))
         (message msg))
        (t (message msg))))

(defun sb-update-buffer (package buffer)
  "Makes changes to the shared buffer specified by the package."
  (save-excursion
    (let ((old-curr-buf (current-buffer)))
      (set-buffer buffer)
      (remove-hook 'after-change-functions 'sb-send-update 'local)
      (remove-hook 'post-command-hook 'sb-send-cursor-update 'local)
      (unless (sb-package-for-new-client package)
        (goto-char (sb-package-start package))
        (delete-char (sb-package-bytes package))
        (sb-insert (sb-package-text package))
        (sb-move-cursor (sb-package-id package)
                        (sb-package-cursor package)))
      (add-hook 'after-change-functions 'sb-send-update nil 'local)
      (add-hook 'post-command-hook 'sb-send-cursor-update nil 'local)
      (set-buffer old-curr-buf))))

(defun sb-client-new-connection-filter (process msg)
  "This filter is used when a connection is initiated. During
  most of a shared-buffer-session sb-client-filter will be used."
  (save-excursion
    (let ((old-curr-buf (current-buffer))
          (package (read msg))
          (id (read msg)))

      (set-buffer (process-buffer process))
      (remove-hook 'after-change-functions 'sb-send-update 'local)
      (sb-insert (sb-package-text package))
      (add-hook 'after-change-functions 'sb-send-update nil 'local)
      (set-buffer old-curr-buf)))
  (set-process-filter process 'sb-client-filter))

(defun sb-client-filter (process msg)
  "The filter function handles all messages from the server."
  (if (not (string-match "\\[cl-struct-sb-package" msg))
      (message-from-server process msg)
    (sb-update-buffer (read msg) (process-buffer process))))

(defun sb-client-sentinel (process msg)
  (message msg))
