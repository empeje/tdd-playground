;; Hello world
(format t "Hello, World.~%")
(loop for line = (read-line *terminal-io* nil :eof)
     until (eq line :eof)
     do (write-line line))
