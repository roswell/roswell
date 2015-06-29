package main

type subCommandFnc func(argv []string, cmd subCommand) int

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
