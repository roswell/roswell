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
