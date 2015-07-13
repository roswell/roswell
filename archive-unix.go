package main

import (
	"io"
	"os/exec"
)

func extract(filename string, doExtract bool, flags int, outputpath string) int {
	condPrintf(0, "name:%s,doExtract:%t,flags:%d,outpath:%s\n", filename, doExtract, flags, outputpath)
	typ, len, ret := "gzip", len(filename), -1
	if len > 4 {
		for c, i := 0, len-1; filename[i] != '.' && c < 5; i-- {
			if filename[i] == 'b' || filename[i] == 'B' {
				typ = "bzip2"
				break
			} else if filename[i] == 'x' || filename[i] == 'X' {
				typ = "xz"
				break
			} else if filename[i] == '7' {
				typ = "7za"
				break
			}
			c++
		}
	}
	condPrintf(1, "extracttype=%s\n", typ)
	if typ == "gzip" || typ == "bzip2" || typ == "xz" {
		str1 := []string{typ, "-dc", filename}
		str2 := []string{"tar", "-" + ters(doExtract, "x", "t") + ters(flags != 0, "p", "") + "f", "-", "-C", outputpath}
		c1 := exec.Command(str1[0], str1[1:]...)
		c2 := exec.Command(str2[0], str2[1:]...)
		r, w := io.Pipe()
		c2.Stdin, c1.Stdout = r, w
		c1.Start()
		c2.Start()
		c1.Wait()
		w.Close()
		c2.Wait()
	} else if typ == "7za" {
		ensureDirectoriesExist(outputpath)
		str1 := []string{"7za", ters(doExtract, "x", "t"), "-o" + outputpath, filename}
		c1 := exec.Command(str1[0], str1[1:]...)
		c1.Start()
		c1.Wait()
	}
	return ret
}
