package main

import (
	"fmt"
)

func cmdConfig(argv []string, cmd subCommand) int {
	if len(argv) == 1 {
		fmt.Printf("oneshot:\n")
		printOpts(localOpt)
		fmt.Printf("local:\n")
		printOpts(globalOpt)
	} else {
		path := configdir() + "config"
		if len(argv) == 2 {
			globalOpt = unsetOpt(globalOpt, argv[1])
			saveOpts(path, globalOpt)
		} else if argv[1] == "set" && len(argv) == 4 {
			globalOpt = setOpt(globalOpt, argv[2], argv[3], 0)
			saveOpts(path, globalOpt)
		} else if argv[1] == "show" && len(argv) == 3 {
			fmt.Printf("%s\n", _getOpt(globalOpt, argv[2]))
		} else if len(argv) == 3 {
			globalOpt = setOpt(globalOpt, argv[1], argv[2], 0)
			saveOpts(path, globalOpt)
		}
	}
	return 2
}
