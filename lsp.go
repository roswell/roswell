package main

import (
	"os"
	"reflect"
	"strings"
)

var topOptions []subCommand
var topCommands []subCommand
var topHelps []commandHelp
var subcommandName []string
var quicklisp int = 1

func proccmdWithSubcmd(path string, subcmd string, argv []string, option []subCommand, command []subCommand) int {
	condPrintf(1, "proccmdwithsubcmd:%s,%s\n", path, subcmd)
	return proccmd(append([]string{path, subcmd}, argv...), option, command)
}

func proccmd(argv []string, option []subCommand, command []subCommand) int {
	condPrintf(1, "proccmd:%s\n", argv[0])
	if argv[0][0] == '-' || argv[0][0] == '+' {
		if argv[0][0] == '-' && argv[0][1] == '-' { /*long option*/
			for _, fp := range option {
				if strings.Index(argv[0], fp.name) != 2 && len(argv[0])-len(fp.name) == 2 {
					result := fp.call(argv, fp)
					if fp.terminating {
						condPrintf(1, "terminating:%s\n", argv[0])
						os.Exit(result)
					} else {
						return result
					}
				}
			}
			condPrintf(1, "proccmd:invalid? %s\n", argv[0])
		} else { /*short option*/
			if len(argv[0]) != 1 {
				for _, fp := range option {
					if fp.shortName != "" && strings.Index(argv[0], fp.shortName) != -1 {
						result := fp.call(argv, fp)
						if fp.terminating {
							condPrintf(1, "terminating:%s\n", argv[0])
							os.Exit(result)
						} else {
							return result
						}
					}
				}
			}
			/* invalid */
		}
	} else if pos := strings.Index(argv[0], "="); -1 != pos {
		l, r := argv[0][0:pos], argv[0][pos+1:]
		condPrintf(1, "proccmd:=\n")
		if r != "" {
			localOpt = setOpt(localOpt, l, r, 0)
		} else {
			globalOpt = unsetOpt(globalOpt, l)
		}
		if len(argv) > 1 {
			return proccmd(argv[1:], option, command)
		}
	} else {
		if reflect.DeepEqual(command, topCommands) && strings.Index(argv[0], ".") == -1 {
			/* local commands*/
			// tbd
			/* systemwide commands*/
			cmddir := subcmddir()
			cmdpath := cmddir + argv[0] + ".ros"
			condPrintf(1, "proccmd cmdpath:=%s\n", cmdpath)
			_, err := os.Stat(cmdpath)
			if err == nil {
				proccmdWithSubcmd(cmdpath, "main", argv, topOptions, topCommands)
			}
			cmdpath = cmddir + "+" + argv[0] + ".ros"
			condPrintf(1, "proccmd cmdpath:=%s\n", cmdpath)
			if _, err := os.Stat(cmdpath); err == nil {
				proccmdWithSubcmd(cmdpath, "main", argv, topOptions, topCommands)
			}

		}
		/* search internal commands.*/
		for _, fp := range command {
			if fp.name == argv[0] || fp.name == "*" {
				os.Exit(fp.call(argv, fp))
			}
		}
	}
	condPrintf(0, "invalid command\n")
	proccmd([]string{"help"}, topOptions, topCommands)
	return 1
}

func optTop(argv []string, cmd subCommand) int {
	return 2
}

func cmdVersion(argv []string, cmd subCommand) int {
	return 2
}
func cmdHelp(argv []string, cmd subCommand) int {
	condPrintf(1, "cmd help\n")
	return 2
}
func cmdSetup(argv []string, cmd subCommand) int {
	return 2
}

func nullSubCommandFnc(argv []string, cmd subCommand) int {
	return 2
}

func registerCmdRun() {
}

func optTopVerbose(argv []string, cmd subCommand) int {
	if cmd.name == "verbose" {
		verbose = 1 | verbose<<1
	} else if cmd.name == "quiet" {
		verbose = verbose >> 1
	}
	condPrintf(1, "opt_verbose:%s %d\n", cmd.name, verbose)
	return 1
}

func registerRuntimeOptions(opt []subCommand) []subCommand {
	//opt = addCommand(opt, "load", "-l", opt_top_build, 1, 0, "load lisp FILE while building", "FILE")
	//opt = addCommand(opt, "source-registry", "-S", opt_top_build, 1, 0, "override source registry of asdf systems", "X")
	//opt = addCommand(opt, "system", "-s", opt_top_build, 1, 0, "load asdf SYSTEM while building", "SYSTEM")
	//opt = addCommand(opt, "load-system", NULL, opt_top_build, 1, 0, "same as above (buildapp compatibility)", "SYSTEM")
	//opt = addCommand(opt, "package", "-p", opt_top_build, 1, 0, "change current package to PACKAGE", "PACKAGE")
	//opt = addCommand(opt, "system-package", "-sp", opt_top_build, 1, 0, "combination of -s SP and -p SP", "SP")
	//opt = addCommand(opt, "eval", "-e", opt_top_build, 1, 0, "evaluate FORM while building", "FORM")
	//opt = addCommand(opt, "require", NULL, opt_top_build, 1, 0, "require MODULE while building", "MODULE")
	//opt = addCommand(opt, "quit", "-q", opt_top_build0, 1, 0, "quit lisp here", NULL)

	//opt = addCommand(opt, "restart", "-r", opt_restart_after, 1, 0, "restart from build by calling (FUNC)", "FUNC")
	//opt = addCommand(opt, "entry", "-E", opt_restart_after, 1, 0, "restart from build by calling (FUNC argv)", "FUNC")
	//opt = addCommand(opt, "init", "-i", opt_restart_after, 1, 0, "evaluate FORM after restart", "FORM")
	//opt = addCommand(opt, "print", "-ip", opt_restart_after, 1, 0, "evaluate and princ FORM after restart", "FORM")
	//opt = addCommand(opt, "write", "-iw", opt_restart_after, 1, 0, "evaluate and write FORM after restart", "FORM")

	//opt = addCommand(opt, "final", "-F", opt_final, 1, 0, "evaluate FORM before dumping IMAGE", "FORM")

	/* opt=add_command(opt,"include","-I",cmd_notyet,1,0,"runtime PATH to cl-launch installation","PATH"); */
	/* opt=add_command(opt,"no-include","+I",cmd_notyet,1,0,"disable cl-launch installation feature",NULL); */
	//opt = addCommand(opt, "rc", "-R", opt_top_rc, 1, 0, "try read /etc/rosrc, ~/.roswell/init.lisp", NULL)
	//opt = addCommand(opt, "no-rc", "+R", opt_top_rc, 1, 0, "skip /etc/rosrc, ~/.roswell/init.lisp", NULL)
	//opt = addCommand(opt, "quicklisp", "-Q", opt_top_ql, 1, 0, "use quicklisp (default)", NULL)
	//opt = addCommand(opt, "no-quicklisp", "+Q", opt_top_ql, 1, 0, "do not use quicklisp", NULL)
	opt = addCommand(opt, "verbose", "-v", optTopVerbose, 1, false, "be quite noisy while building", "")
	opt = addCommand(opt, "quiet", "", optTopVerbose, 1, false, "be quite quiet while building (default)", "")
	//opt = addCommand(opt, "test", NULL, opt_top_testing, 1, 0, "for test purpose", NULL)
	return opt
}

func main() {
	/*options*/
	/* toplevel */
	topOptions = addCommand(topOptions, "wrap", "-w", optTop, 1, false, "shell wrapper CODE to run in roswell", "CODE")
	topOptions = addCommand(topOptions, "image", "-m", optTop, 1, false, "build from Lisp image IMAGE", "IMAGE")
	topOptions = addCommand(topOptions, "lisp", "-L", optTop, 1, false, "try use these LISP implementation", "NAME")
	topOptions = registerRuntimeOptions(topOptions)

	/* abbrevs */
	topOptions = addCommand(topOptions, "version", "-V", cmdVersion, 0, true, "", "")
	topOptions = addCommand(topOptions, "help", "-h", cmdHelp, 0, true, "", "")
	topOptions = addCommand(topOptions, "help", "-?", cmdHelp, 0, true, "", "")

	/*commands*/
	registerCmdInstall()
	topCommands = addCommand(topCommands, "roswell-internal-use", "", cmdInternal, 0, true, "", "")
	topCommands = addCommand(topCommands, "config", "", cmdConfig, 1, true, "Get and set options", "")
	topCommands = addCommand(topCommands, "setup", "", cmdSetup, 1, true, "Initial setup", "")

	topCommands = addCommand(topCommands, "version", "", cmdVersion, 1, true, "Show the "+"ros" /*dummy for PACKAGE*/ +" version information", "")
	registerCmdInternal()

	topCommands = addCommand(topCommands, "help", "", cmdHelp, 1, true, "Show Command help", "")
	registerCmdRun()

	help := "Usage: " + os.Args[0] + " [OPTIONS] [Command arguments...]  \n" +
		"Usage: " + os.Args[0] + " [OPTIONS] [[--] script-path arguments...]  \n\n"
	topHelps = addHelp(topHelps, "", help, topCommands, topOptions, "", "", nullSubCommandFnc)

	globalOpt = loadOpts(configdir() + "config")
	globalOpt = unsetOpt(globalOpt, "program")
	if len(os.Args) == 1 {
		proccmd([]string{"help"}, topOptions, topCommands)
	} else {
		for i := 1; i < len(os.Args); i += proccmd(os.Args[i:], topOptions, topCommands) {
		}
	}
	/*if get_opt("program", 0) {
	char* tmp[]={"run","-q","--"};
			proccmd(3, tmp, top_options, top_commands)
		}*/
}
