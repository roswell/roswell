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
type installCmds func(param *installOptions) int
type installImpls struct {
	name      string
	call      []installCmds
	uri       int // dummy
	extention int //dummy
	util      bool
}

var impls_quicklisp installImpls
var impls_to_install = []*installImpls{&impls_sbcl_bin, &impls_quicklisp}

func cmdInstallInternal(impl *installImpls, param *installOptions) {
	var ret int
	for _, fun := range impl.call {
		ret = fun(param)
		if ret == 0 {
			break
		}
	}
	if ret == 0 { // after install latest installed impl/version should be default for 'run'
		path := configdir() + "config"
		v := param.impl + ".version"
		version := param.version
		if !impl.util {
			for i := 0; i < len(version); i++ {
				if version[i] == '-' {
					version = version[0:i]
				}
			}
			globalOpt = setOpt(globalOpt, "default.lisp", param.impl, 0)
			globalOpt = setOpt(globalOpt, v, version, 0)
			saveOpts(path, globalOpt)
		}
	} else {
		os.Exit(1)
	}
}

func cmdInstall(argv []string, cmd subCommand) (ret int) {
	//tbd
	var param installOptions
	quicklisp = 1
	param.os = uname()
	param.arch = uname_m()
	param.archInArchiveName = false
	param.expandPath = ""
	//condPrintf(1, "argv:%s:%d\n", argv, len(argv))
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
		for _, impl := range impls_to_install {
			if param.impl == impl.name {
				cmdInstallInternal(impl, &param)
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
