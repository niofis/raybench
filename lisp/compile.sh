cd lisp 
sbcl --disable-debugger --eval '(compile-file "lisprb.lisp")' --eval '(load "lisprb.fasl")' --eval '(lisprb::dump)'
