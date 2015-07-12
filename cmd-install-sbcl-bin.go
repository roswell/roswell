package main

import ()

func arch_(param *installOptions) string {
	return param.arch + "-" + param.os
}

func sbclVersionBin(param *installOptions) int {
	platforms_html := configdir() + "tmp" + SLASH + "sbcl-bin.html"
	ensureDirectoriesExist(platforms_html)
	if param.version == "" {
		condPrintf(0, "SBCL version is not specified. Downloading platform-table.html to see which version to install...\n")
		ret := downloadSimple("http://www.sbcl.org/platform-table.html", platforms_html, 0)
		if ret != 0 {
			condPrintf(0, "Download failed (Code=%d)\n", ret)
			return 0
		}
		param.version = sbclBin(platforms_html)
		condPrintf(0, "Installing sbcl-bin/%s...\n", param.version)
	}
	param.archInArchiveName = true
	param.expandPath = configdir() + "src" + SLASH + "sbcl" + "-" + param.version + "-" + arch_(param) + SLASH
	return 1
}

func sbcl_bin_uri(param *installOptions) string {
	return "http://prdownloads.sourceforge.net/sbcl/sbcl-" + param.version + "-" + arch_(param) + "-binary" + sbcl_bin_extention(param)
}

var impls_sbcl_bin = installImpls{
	name: "sbcl-bin",
	call: []installCmds{
		sbclVersionBin,
		start,
		download,
		sbclBinExpand,
		sbclBinInstall,
	},
	extention: sbcl_bin_extention,
	uri:       sbcl_bin_uri,
}
