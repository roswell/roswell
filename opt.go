package main

var localOpt []opts
var globalOpt []opts

type subCommandFnc func(argv []string, cmd subCommand) int

type opts struct {
	name  string
	typ   int
	value string
}

type subCommand struct {
	name        string
	shortName   string
	call        subCommandFnc
	showOpt     int
	terminating bool
	description string
	argExample  string
}

type commandHelp struct {
	name     string
	usage    string
	commands []subCommand
	opts     []subCommand
	header   string
	footer   string
	call     subCommandFnc
}

func addHelp(base []commandHelp, name string, usage string, commands []subCommand,
	opts []subCommand, header string, footer string, call subCommandFnc) []commandHelp {
	return append(base, commandHelp{name: name, usage: usage,
		commands: commands, opts: opts, header: header, footer: footer, call: call})
}

func addCommand(base []subCommand, name string, shortName string, call subCommandFnc,
	showOpt int, terminating bool, description string, argExample string) []subCommand {

	return append(base, subCommand{name: name, shortName: shortName, call: call, showOpt: showOpt,
		terminating: terminating, description: description, argExample: argExample})
}

func setOpt(opt []opts, name string, value string, typ int) []opts {
	//tbd
	condPrintf(1, "setOpt:\n")
	return []opts{}
}

func unsetOpt(opt []opts, name string) []opts {
	//tbd
	condPrintf(1, "unsetOpt:\n")
	return []opts{}
}
