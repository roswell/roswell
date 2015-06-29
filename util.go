package main

import (
	"fmt"
	"os"
)

var verbose int

func condPrintf(v int, format string, args ...interface{}) {
	if v == v&verbose {
		fmt.Fprintf(os.Stderr, format, args...)
	}
}
