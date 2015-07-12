package main

import ()

func extract(filename string, doExtract bool, flags int, outputpath string) int {
	condPrintf(0, "name:%s,doExtract:%t,flags:%d,outpath:%s\n", filename, doExtract, flags, outputpath)
	typ, str, len, ret := "gzip", "", len(filename), -1
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
		str = typ + " -dc " + filename + " | tar -" + ters(doExtract, "x", "t") +
			ters(flags != 0, "p", "") + "f - -C " + outputpath
	} else if typ == "7za" {
		ensureDirectoriesExist(outputpath)
		str = "7za " + ters(doExtract, "x", "t") + " -o" + outputpath + " " + filename
	}
	condPrintf(1, "extractcmd=%s\n", str)
	if str != "" {
		_, ret = system_(str)
	}
	return ret
}
