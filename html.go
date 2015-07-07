package main

import (
	"bufio"
	"golang.org/x/net/html"
	"os"
	"strings"
)

func sbclBin(path string) string {
	u := uname_m() + "-" + uname()
	condPrintf(1, "open %s\n", path)
	in, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer in.Close()
	r := bufio.NewReaderSize(in, 4096)
	doc, err := html.Parse(r)
	if err != nil {
		panic(err)
	}
	var f func(n *html.Node) string
	f = func(n *html.Node) string {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, attr := range n.Attr {
				if k, v := attr.Key, attr.Val; k == "href" && (v[len(v)-3:] == "bz2" || v[len(v)-3:] == "msi") && strings.Index(v, u) != -1 {
					return v
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			if result := f(c); result != "" {
				return result
			}
		}
		return ""
	}
	return f(doc)
}
