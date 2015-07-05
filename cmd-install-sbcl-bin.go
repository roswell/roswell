package main

import ()

func sbclVersionBin(param *installOptions) int {
	platforms_html := configdir() + "tmp" + SLASH + "sbcl-bin.html"
	ensureDirectoriesExist(platforms_html)
	if param.version == "" {
		condPrintf(0, "SBCL version is not specified. Downloading platform-table.html to see which version to install...\n")
	}
	return 1
}

func dum1(param *installOptions) int {
	condPrintf(1, "hoge-----1\n")
	return 0
}

var impls_sbcl_bin = installImpls{
	name: "sbcl-bin",
	call: []installCmds{
		sbclVersionBin,
		dum1,
	}}
