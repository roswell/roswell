package main

type subCommand struct {
	name        string
	shortName   string
	call        int /*dummy untill learn howto use function in go*/
	showOpt     int
	terminating int
	description string
	argExample  string
}

func AddCommand(base []subCommand, name string, shortName string, call int,
	showOpt int, terminating int, description string, argExample string) []subCommand {

	return append(base, subCommand{name: name, shortName: shortName, call: call, showOpt: showOpt,
		terminating: terminating, description: description, argExample: argExample})
}
