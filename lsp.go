package main

import (
	"fmt"
	"os"
)

var topOptions []subCommand

func Proccmd(argv []string, option []string, command []string) int {
	return 1
}

func main() {
	/*options*/
	/* toplevel */
	topOptions = AddCommand(topOptions, "wrap", "-w", 0 /*dummy*/, 1, 0, "shell wrapper CODE to run in roswell", "CODE")
	topOptions = AddCommand(topOptions, "image", "-m", 0 /*dummy*/, 1, 0, "build from Lisp image IMAGE", "IMAGE")
	topOptions = AddCommand(topOptions, "lisp", "-L", 0 /*dummy*/, 1, 0, "try use these LISP implementation", "NAME")

	if len(os.Args) == 1 {
		fmt.Print("help")
	} else {
		for i := 1; i < len(os.Args); i += Proccmd(os.Args, []string{"dummy"}, []string{"dummy"}) {
		}
	}
}
