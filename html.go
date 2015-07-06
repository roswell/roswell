package main

import (
	"bufio"
	"os"
)

type tag struct {
	name  string
	typ   int
	value string
	attr  []tag
}

func parseTags(in *bufio.Reader, tags []tag, a int) []tag {
	return []tag{}
}

func atagList(path string) []tag {
	condPrintf(1, "open %s\n", path)
	in, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer in.Close()
	reader := bufio.NewReaderSize(in, 4096)
	rune, _, _ := reader.ReadRune()
	condPrintf(1, "%d", rune)
	return parseTags(reader, []tag{}, 0)
}

func sbclBin(path string) string {
	return ""
}
