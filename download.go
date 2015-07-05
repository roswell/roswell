package main

import (
	"io"
	"net/http"
	"os"
)

func downloadSimple(uri string, path string, verbose bool) int {
	out, _ := os.Create(path)
	defer out.Close()
	resp, _ := http.Get(uri)
	defer resp.Body.Close()
	io.Copy(out, resp.Body)
	return 0
}
