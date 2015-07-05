package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

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

func printOpts(opt []opts) {
	for _, op := range opt {
		fmt.Printf("%s=%s[]\n", op.name, op.value)
	}
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
	for _, v := range opt {
		if v.name == name {
			v.value = value
			return opt
		}
	}
	return append(opt, opts{name: name, value: value, typ: 0})
}

func _getOpt(opt []opts, name string) string {
	for _, v := range opt {
		if v.name == name {
			return v.value
		}
	}
	return ""
}

func unsetOpt(opt []opts, name string) []opts {
	for i, v := range opt {
		if v.name == name {
			return append(opt[0:i], opt[i+1:]...)
		}
	}
	return opt
}
func loadOpts(path string) (ret []opts) {
	f, err := os.Open(path)
	if err != nil {
		return []opts{}
	}
	defer f.Close()
	reader := bufio.NewReaderSize(f, 1024)
	for {
		cur := opts{}
		line, _, err := reader.ReadLine()
		buf := string(line)
		if buf != "" {
			for i, mode, last := 0, 0, 0; i < len(buf); i++ {
				if buf[i] == '\t' || buf[i] == '\n' || len(buf)-1 == i {
					mode++
					switch mode {
					case 1:
						cur.name = buf[last:i]
					case 2:
						//cur.typ = buf[i-1] - '0'
					case 3:
						cur.value = buf[last:]
					}
					last = i + 1
				}

			}
			ret = append(ret, cur)
		}
		if err == io.EOF {
			break
		} else if err != nil {
			panic(err)
		}
	}
	return
}

func saveOpts(path string, opt []opts) int {
	f, err := os.Create(path)
	if err != nil {
		return 0
	}
	defer f.Close()
	for _, op := range opt {
		f.WriteString(fmt.Sprintf("%s\t%d\t%s\n", op.name, 0, op.value))
	}
	return 1
}
