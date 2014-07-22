#!/usr/bin/zsh

screen -L sbcl --load shared-buffer/shared-buffer-server.lisp --eval "(shared-buffer-server:create-server *server* 0.0.0.0)"
