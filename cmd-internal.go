package main

import ()

var internalCommands []subCommand

func cmdDownload(argv []string, cmd subCommand) int {
	if len(argv) >= 2 {
		condPrintf(0, "Downloading %s\n", argv[1])
		return downloadSimple(argv[1], argv[2], 0)
	}
	return 0
}

func cmdTar(argv []string, cmd subCommand) int {
	return 0
}

func cmdUname(argv []string, cmd subCommand) int {
	return 0
}

func cmdWhich(argv []string, cmd subCommand) int {
	condPrintf(0, "which:\n")
	return 0
}

func registerCmdInternal() {
	cmds := internalCommands
	cmds = addCommand(cmds, "tar", "", cmdTar, 0, true, "", "")
	cmds = addCommand(cmds, "download", "", cmdDownload, 0, true, "", "")
	cmds = addCommand(cmds, "uname", "", cmdUname, 0, true, "", "")
	cmds = addCommand(cmds, "which", "", cmdWhich, 0, true, "", "")
	internalCommands = cmds
}

func cmdInternal(argv []string, cmd subCommand) int {
	return proccmd(argv[1:], []subCommand{}, internalCommands)
}
