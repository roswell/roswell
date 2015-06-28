package main

import (
	"fmt"
	"os"
)

func Proccmd(argv []string, option []string, command []string) int {
	return 1
}

func main() {
	if len(os.Args) == 1 {
		fmt.Print("help")
	} else {
		for i := 1; i < len(os.Args); i += Proccmd(os.Args, []string{"dummy"}, []string{"dummy"}) {
		}
	}
}
