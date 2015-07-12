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
	filename, outputpath := "", ""
	mode, flags := 'x', 0
	for i := 1; i < len(argv) && argv[i][0] == '-'; i++ {
		p := argv[i]
		for j, len := 0, len(p); j < len; j++ {
			opt := p[j]
			switch opt {
			case 'f':
				if j != len-1 {
					filename = p[j+1:]
				} else {
					i++
					filename = argv[i]
				}
				j = len
			case 'C':
				if j != len-1 {
					outputpath = p[j+1:]
				} else {
					i++
					outputpath = argv[i]
				}
				j = len
			case 'p':
				flags = 1
			case 't':
				mode = 't'
			case 'v':
				verbose = 1 | verbose<<1
			case 'x':
				mode = 'x'
			}
		}
	}
	switch mode {
	case 't':
		extract(filename, false, flags, outputpath)
	case 'x':
		extract(filename, true, flags, outputpath)
	}
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
