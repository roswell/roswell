package main

import (
	"os"
	"strings"
)

type installOptions struct {
	impl              string
	version           string
	os                string
	arch              string
	archInArchiveName bool
	opt               int    // dummy
	expandPath        string //expand dist
}

type install_impls struct {
	name      string
	call      int //dummy
	uri       int // dummy
	extention int //dummy
	util      int
}

func cmdInstall(argv []string, cmd subCommand) (ret int) {
	//tbd
	var param installOptions
	quicklisp = 1
	param.os = uname()
	param.arch = uname_m()
	param.archInArchiveName = false
	param.expandPath = ""
//	condPrintf(1, "argv:%s:%d\n", argv, len(argv))
	if len(argv) != 1 {
		for k := 1; k < len(argv); k++ {
			param.impl = argv[k]
			if pos := strings.Index(argv[k], "/"); pos != -1 {
				param.version = param.impl[pos+1:]
				param.impl = param.impl[0:pos]
			} else {
				param.version = ""
			}
		}

	} else {
		proccmd([]string{"help", "install"}, topOptions, topCommands)
		os.Exit(0)
	}
	return 0
}

func installHelp(argv []string, cmd subCommand) int {
	//dummy
	return 2
}

func registerCmdInternal() {
	topCommands = addCommand(topCommands, "install", "", cmdInstall, 1, true, "Install archive and build it for "+PACKAGE+" environment", "")
	topHelps = addHelp(topHelps, "install", "", nil, nil, "", "", installHelp)
}
