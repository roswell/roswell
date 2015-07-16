// +build !windows
package main

import (
	"os"
)

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
	version, impl := param.version, param.impl
	implPath := configdir() + "impls" + SLASH + param.arch + SLASH + param.os + SLASH + impl + SLASH + version
	logPath := configdir() + "impls" + SLASH + "log" + SLASH + impl + "-" + version + SLASH + "install.log"
	src := param.expandPath
	sbclHome := implPath + SLASH + "lib" + SLASH + "sbcl"
	installRoot := implPath
	condPrintf(0, "Building %s/%s...", impl, version)
	ensureDirectoriesExist(implPath)
	ensureDirectoriesExist(logPath)
	os.Chdir(src)
	os.Setenv("SBCL_HOME", sbclHome)
	os.Setenv("INSTALL_ROOT", installRoot)
	return 0
}
