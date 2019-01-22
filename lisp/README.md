
to launch with sbcl:<br/>
$>sbcl --script gameOfLife.lisp $(tput cols) $(($(tput lines) - 1)) 15
