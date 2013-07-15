;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.

(require 's)

(cl-defstruct sb-package start bytes text)

(defconst *sb-port* 3705
  "Shared-buffer uses port 3705.")

(defvar *sb-server* nil
  "A buffer-local variable pointing to the server a shared buffer is
  connected to.")

(defvar *sb-change* nil
  "A buffer-local variable indecating if a change was done by
  shared-buffer. This is necessary because shared-buffer uses
  after-change-functions to send updates to the server, and if a change was
  invoked by shared-buffer it should not be sent back to the server.")

(defun sb-connect-to-server (host &optional buffer)
  "This function opens a connection to a shared-buffer server, by starting a
shared-buffer client."
  (interactive "sHost: ")
  (make-local-variable '*sb-server*)
  (make-local-variable '*sb-change*)
  (let ((key (read-from-minibuffer "key: ")))
    (setq *sb-server*
          (make-network-process
           :name "sb-client" :buffer (or buffer (current-buffer))
           :filter 'sb-client-filter :sentinel 'sb-client-sentinel
           :family 'ipv4 :host host :service *sb-port*))
    (process-send-string *sb-server* (s-concat key "\n"))
    (add-hook 'after-change-functions 'sb-update nil 'local)))

(defun sb-update (start end bytes)
  "Sends an update to the server. This function is locally added
to the 'after-change-functions hook for shared buffers."
  (unless *sb-change*
    (let ((package (make-sb-package :start start :bytes bytes :text
                                    (buffer-substring start end))))
      (process-send-string *sb-server*
                           (s-concat (prin1-to-string package) "\n"))))
  (setq *sb-change* nil))

(defun sb-close ()
  "Closes the connecton to the server."
  (interactive)
  (delete-process *sb-server*)
  (setq *sb-server* nil)
  (remove-hook 'after-change-functions 'sb-update 'local))

(defun sb-client-filter (process msg)
  "The filter function handles all messages from the server."
  (message msg)
  (save-excursion
    (let ((buffer (current-buffer))
          (package (read msg)))
      (set-buffer (process-buffer process))
      (goto-char (sb-package-start package))
      (delete-char (sb-package-bytes package))
      (insert (sb-package-text package))
      (setq *sb-change* t)
      (set-buffer buffer))))

(defun sb-client-sentinel (process msg)
  (message msg))
