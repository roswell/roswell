package main

import (
	"errors"
	"io"
	"net/http"
	"os"
)

func downloadSimple(uri string, path string, verbose int) int {
	client := &http.Client{
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			req.Header.Del("Referer")
			if len(via) >= 10 {
				return errors.New("stopped after 10 redirects")
			}
			return nil
		},
	}
	condPrintf(0, "download-url:%s:\ndownload-file:%s\n", uri, path)
	out, _ := os.Create(path)
	defer out.Close()
	resp, _ := client.Get(uri)
	defer resp.Body.Close()
	condPrintf(1, "status:%s:\nheader:%s\n", resp.Status, resp.Header)
	io.Copy(out, resp.Body)
	return 0
}
