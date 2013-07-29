##Warning:
###Still very much in development!

Shared buffer is still at an early development stage, and is probably
riddled with bugs. Yet I want to encourage you to test it out and provide
feedback!

#Shared buffer

Shared buffer is an Emacs extension that enables online collaborative
editing for Emacs.

##Quick-start
###Client side

To install the Emacs extension just download the `shared-buffer.el` and
store it in your `load-path`.

    git clone https://github.com/larstvei/shared-buffer.git

Once loaded you can start sharing a buffer by interactively running the
command:

    M-x sb-share-this-buffer <ret>
    Host: some-address <ret>
    Key: this-is-a-key <ret>

Unless an error-message occurs you will be sending updates to the server
(located on the host provided) at every change in the buffer.

To connect to a shared buffer, run the following command:

    M-x sb-connect-to-shared-buffer <ret>
    Host: some-address <ret>
    Key: this-is-a-key <ret>

Assuming that someone has shared their buffer with the provided host and key
a new buffer will spawn containing whatever content is in the corresponding
buffer.

###Server side

You will need a common lisp interpreter and quicklisp installed. SBCL is
recommended. Download the `shared-buffer-server.lisp` and store it anywhere
you like.

    git clone https://github.com/larstvei/shared-buffer.git


All you need to do is spawn a common lisp interpreter and run:

    CL-USER> (load "shared-buffer-server.lisp")
    CL-USER> (defvar *server* (shared-buffer-server "some-address"))

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
