#Shared buffer

Shared buffer is an Emacs extension that enables online collaborative
editing for Emacs.

It is still at an experimental stage. I encourage you test it out, but make
sure to back up files in case it breaks. There are still quite a fiew bugs.

There is a server running on `virvel.de` that you may use. You
can also set up a server yourself. This is especially useful if you want to
share a buffer with someone who is on the same local network.

##Quick-start

To install the Emacs extension just download the `shared-buffer.el` and
store it in your `load-path`.

    git clone https://github.com/larstvei/shared-buffer.git

Once loaded you can start sharing a buffer by interactively running the
command:

    M-x sb-share-this-buffer RET
    Host: virvel.de RET
    Key: this-is-a-key RET

The key is arbitrary, share it (i.e. by email) with the ones you want to
share a buffer with. Unless an error-message occurs you will be sending
updates to the server (located on the host provided) at every change in the
buffer.

To connect to a shared buffer, run the following command:

    M-x sb-connect-to-shared-buffer RET
    Host: virvel.de RET
    Key: this-is-a-key RET

Assuming that someone has shared their buffer with the provided host and key
a new buffer will spawn containing whatever content is in the corresponding
buffer. You can save the buffer to whatever file you choose, and use
whatever mode you want.

You can disconnect by interactively running:

    M-x sb-disconnect

or simply killing the buffer.

##Setting up a server

You will need a common lisp interpreter and quicklisp installed. SBCL is the
only implementation that has been tested, and therefor also
recommended. Download the `shared-buffer-server.lisp` and store it anywhere
you like.

    git clone https://github.com/larstvei/shared-buffer.git

All you need to do is spawn a common lisp interpreter and run:

    CL-USER> (load "shared-buffer-server.lisp")

It will set up a server on address `0.0.0.0` and port `3705`. To change this
you can edit two lines of the code.

```lisp
(defconstant +port+ 3705
  "Shared-buffer uses port 3705.")
  
(defvar *server* (shared-buffer-server "0.0.0.0"))
```
To stop the server run:

    CL-USER> (SB-THREAD:destroy-thread *server*)

##How it works

You may either share the content of an arbitrary buffer or connect to a
buffer that is already shared. When starting a shared buffer session you
must choose a *host* (an address to a shared-buffer-server) and a *key*. To
connect to a buffer that is already shared one must provide a *host* and a
*key*. If this *key* matches a shared buffer on the chosen host a new buffer
will spawn containing whatever content is in the corresponding buffer. A
shared buffer stays shared as long as there are users connected to it (this
means the one who initiated the shared-buffer-session has no special
privileges).

There is no additional version control system, because you *don't share
files*! You only share temporarily stored text (hence shared *buffer*). Each
user is responsible to save the buffer to whatever file he/she may
choose.

##TODO

* There are synchronization problems. If they emerge, there is currently no
  other solution than to disconnect and reconnecting.
* Implement a mode.
* Implement a chat feature.
* Make more user friendly.
