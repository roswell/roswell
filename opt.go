package main

type subCommand struct {
	name        string
	shortName   string
	call        int
	showOpt     int
	terminating int
	description string
	argExample  string
}

func AddCommand(base []subCommand, name string, shortName string, fnc int,
	showOpt int, terminating int, description string, argExample string) []subCommand {

	return []subCommand{}
}
