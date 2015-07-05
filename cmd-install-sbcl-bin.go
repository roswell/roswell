package main

import ()

func sbclVersionBin(param *installOptions) int {
	platforms_html := configdir() + "tmp" + SLASH + "sbcl-bin.html"
	ensureDirectoriesExist(platforms_html)
	if param.version == "" {
		condPrintf(0, "SBCL version is not specified. Downloading platform-table.html to see which version to install...\n")
		ret := downloadSimple("http://www.sbcl.org/platform-table.html", platforms_html, false)
		if ret != 0 {
			condPrintf(0, "Download failed (Code=%d)\n", ret)
			return 0
		}
	}
	return 1
}

func dum1(param *installOptions) int {
	condPrintf(1, "hoge-----:%s:\n", param.version)
	return 0
}

var impls_sbcl_bin = installImpls{
	name: "sbcl-bin",
	call: []installCmds{
		sbclVersionBin,
		dum1,
	}}
