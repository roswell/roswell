package main

import (
	"fmt"
	"os"
)

var topOptions []subCommand
var topCommands []subCommand
var topHelps []commandHelp

func Proccmd(argv []string, option []string, command []string) int {
	return 1
}

func optTop(argv []string, cmd []subCommand) int {
	return 2
}

func cmdVersion(argv []string, cmd []subCommand) int {
	return 2
}
func cmdHelp(argv []string, cmd []subCommand) int {
	return 2
}
func cmdInternal(argv []string, cmd []subCommand) int {
	return 2
}
func cmdConfig(argv []string, cmd []subCommand) int {
	return 2
}
func cmdSetup(argv []string, cmd []subCommand) int {
	return 2
}
func nullSubCommandFnc(argv []string, cmd []subCommand) int {
	return 2
}

func registerCmdInstall() {
}

func registerCmdRun() {
}
func registerCmdInternal() {
}

func main() {
	/*options*/
	/* toplevel */
	topOptions = addCommand(topOptions, "wrap", "-w", optTop, 1, 0, "shell wrapper CODE to run in roswell", "CODE")
	topOptions = addCommand(topOptions, "image", "-m", optTop, 1, 0, "build from Lisp image IMAGE", "IMAGE")
	topOptions = addCommand(topOptions, "lisp", "-L", optTop, 1, 0, "try use these LISP implementation", "NAME")
	//topOptions = registerRuntimeOptions(topOptions)

	/* abbrevs */
	topOptions = addCommand(topOptions, "version", "-V", cmdVersion, 0, 1, "", "")
	topOptions = addCommand(topOptions, "help", "-h", cmdHelp, 0, 1, "", "")
	topOptions = addCommand(topOptions, "help", "-?", cmdHelp, 0, 1, "", "")

	/*commands*/
	registerCmdInstall()
	topCommands = addCommand(topCommands, "roswell-internal-use", "", cmdInternal, 0, 1, "", "")
	topCommands = addCommand(topCommands, "config", "", cmdConfig, 1, 1, "Get and set options", "")
	topCommands = addCommand(topCommands, "setup", "", cmdSetup, 1, 1, "Initial setup", "")

	topCommands = addCommand(topCommands, "version", "", cmdVersion, 1, 1, "Show the "+"ros" /*dummy for PACKAGE*/ +" version information", "")
	registerCmdInternal()

	topCommands = addCommand(topCommands, "help", "", cmdHelp, 1, 1, "Show Command help", "")
	registerCmdRun()

	help := "Usage: " + os.Args[0] + " [OPTIONS] [Command arguments...]  \n" +
		"Usage: " + os.Args[0] + " [OPTIONS] [[--] script-path arguments...]  \n\n"
	topHelps = addHelp(topHelps, "", help, topCommands, topOptions, "", "", nullSubCommandFnc)

	/*
				char * path = s_cat(configdir(), q("config"), NULL)
				global_opt = load_opts(path)
	struct opts** opts=&global_opt;
				unset_opt(opts, "program")
	*/
	if len(os.Args) == 1 {
		fmt.Print("help")
	} else {
		for i := 1; i < len(os.Args); i += Proccmd(os.Args, []string{"dummy"}, []string{"dummy"}) {
		}
	}
	/*if get_opt("program", 0) {
	char* tmp[]={"run","-q","--"};
			proccmd(3, tmp, top_options, top_commands)
		}*/
}
