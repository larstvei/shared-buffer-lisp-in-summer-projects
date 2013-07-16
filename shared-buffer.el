;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.

(require 's)
(require 'dash)

(cl-defstruct sb-package start bytes text)

(defconst *sb-port* 3705
  "Shared-buffer uses port 3705.")

(defvar *sb-server* nil
  "A buffer-local variable pointing to the server a shared buffer is
  connected to.")

(defun sb-connect-to-server (host &optional buffer)
  "This function opens a connection to a shared-buffer server, by starting a
shared-buffer client."
  (interactive "sHost: ")
  (make-local-variable '*sb-server*)
  (let ((key (read-from-minibuffer "key: ")))
    (setq *sb-server*
          (make-network-process
           :name "sb-client" :buffer (or buffer (current-buffer))
           :filter 'sb-client-filter :sentinel 'sb-client-sentinel
           :family 'ipv4 :host host :service *sb-port*))
    (process-send-string *sb-server* (concat key "\n"))
    (add-hook 'after-change-functions 'sb-update nil 'local)))

(defun sb-update (start end bytes)
  "Sends an update to the server. This function is locally added
to the 'after-change-functions hook for shared buffers."
  (let ((package
         (make-sb-package
          :start start :bytes bytes
          :text (s-lines (buffer-substring-no-properties start end)))))
    (process-send-string *sb-server*
                         (concat (prin1-to-string package) "\n"))))

(defun sb-close ()
  "Closes the connecton to the server."
  (interactive)
  (delete-process *sb-server*)
  (setq *sb-server* nil)
  (remove-hook 'after-change-functions 'sb-update 'local))

(defun sb-possible-update-p (package)
  (and (<= (+ (sb-package-start package)
              (sb-package-bytes package))
           (point-max))))

(defun sb-insert (strings)
  (insert (car strings))
  (mapc (lambda (str)
          (insert str) (newline)) (cdr strings)))

(defun sb-client-filter (process msg)
  "The filter function handles all messages from the server."
  (message msg)
  (save-excursion
    (let ((buffer (current-buffer))
          (package (read msg)))
      (set-buffer (process-buffer process))
      (remove-hook 'after-change-functions 'sb-update 'local)
      (if (not (sb-possible-update-p package))
          (message "sb-warning: out of sync! Run command: sb-sync")
        (goto-char (sb-package-start package))
        (delete-char (sb-package-bytes package))
        (sb-insert (sb-package-text package)))
      (add-hook 'after-change-functions 'sb-update nil 'local)
      (set-buffer buffer))))

(defun sb-client-sentinel (process msg)
  (message msg))
