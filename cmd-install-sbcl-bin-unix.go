// +build !windows
package main

import ()

func sbcl_bin_extention(param *installOptions) string {
	return ".tar.bz2"
}

func sbclBinExpand(param *installOptions) int {
	argv := []string{"", "-xf", "", "-C", ""}
	archive := downloadArchiveName(param)
	distPath := param.expandPath
	condPrintf(0, "Extracting %s to %s\n", archive, distPath)
	deleteDirectory(distPath, true)
	ensureDirectoriesExist(distPath)
	argv[2] = configdir() + "archives" + SLASH + archive
	argv[4] = configdir() + "src" + SLASH
	if r := cmdTar(argv, subCommand{}); r == 0 {
		return 1
	}
	return 0
}

func sbclBinInstall(param *installOptions) int {
	return 0
}
