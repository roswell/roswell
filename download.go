package main

import (
	"io"
	"net/http"
	"os"
)

func downloadSimple(uri string, path string, verbose int) int {
        condPrintf(0,"download-url:%s:\ndownload-file:%s\n",uri,path)
	out, _ := os.Create(path)
	defer out.Close()
	resp, _ := http.Get(uri)
	defer resp.Body.Close()
        condPrintf(0,"status:%s:\nheader:%s\n",resp.Status,resp.Header)
	io.Copy(out, resp.Body)
	return 0
}
