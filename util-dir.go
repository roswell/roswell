package main

import (
	"log"
	"os"
	"os/user"
	"strings"
)

func subcmddir() string {
	// tbd
	if i := strings.LastIndex(os.Args[0], "/"); i != -1 {
		return os.Args[0][0:i+1] + "src/lisp/"
	}
	return ""
}
func homedir() string {
	usr, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return usr.HomeDir + SLASH
}
func configdir() string {
	if home := homedir(); home != "" {
		return home + "." + PACKAGE + SLASH
	}
	return ""
}

func deleteDirectory(pathspec string, recursive bool) int {
	var e error
	if recursive {
		e = os.RemoveAll(pathspec)
	} else {
		e = os.Remove(pathspec)
	}
	if e != nil {
		return 0
	}
	return 1
}
