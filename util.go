package main

import (
	"fmt"
	"os"
	"strings"
)

var verbose int

func condPrintf(v int, format string, args ...interface{}) {
	if v == v&verbose {
		fmt.Fprintf(os.Stderr, format, args...)
	}
}

func ensureDirectoriesExist(path string) error {
	if pos := strings.LastIndex(path, SLASH); pos != -1 {
		path = path[0 : pos+1]
	}
	return os.MkdirAll(path, 0755)
}

func directoryExistP(path string) bool {
	f, err := os.Stat(path)
	if err != nil {
		return false
	}
	return f.IsDir()
}

func fileExistP(path string) bool {
	f, err := os.Stat(path)
	if err != nil {
		return false
	}
	return f.Mode().IsRegular()
}

func ters(cl bool, then_ string, else_ string) string {
	if cl {
		return then_
	}
	return else_
}
