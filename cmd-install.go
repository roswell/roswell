package main

import (
	"os"
	"strings"
)

type installOptions struct {
	impl              string
	version           string
	os                string
	arch              string
	archInArchiveName bool
	opt               int    // dummy
	expandPath        string //expand dist
}
type installCmds func(param *installOptions) int
type installImpls struct {
	name      string
	call      []installCmds
	uri       func(param *installOptions) string
	extention func(param *installOptions) string
	util      bool
}

var install_impl *installImpls
var impls_quicklisp installImpls
var impls_to_install = []*installImpls{&impls_sbcl_bin, &impls_quicklisp}

func installedP(param *installOptions) bool {
	impl := param.impl
	//TBD for util.
	i := configdir() + "impls" + SLASH + param.arch + SLASH + param.os + SLASH + impl + ters(param.version != "", SLASH, "") + param.version + SLASH
	ret := directoryExistP(i)
	condPrintf(1, "directoryExistP(%s)=%t\n", i, ret)
	return ret
}

func installRunningP(param *installOptions) bool {
	/* TBD */
	return false
}

func start(param *installOptions) int {
	ensureDirectoriesExist(configdir())
	if installedP(param) {
		condPrintf(0, "%s/%s is already installed. Try (TBD) if you intend to reinstall it.\n", param.impl, param.version)
		return 0
	}
	if installRunningP(param) {
		condPrintf(0, "It seems running installation process for $1/$2.\n")
		return 0
	}
	p := configdir() + "tmp" + SLASH + param.impl + ters(param.version != "", "-", "") + param.version + SLASH
	setupSignalHandler(p)
	touch(p)
	return 1
}

func download_archive_name(param *installOptions) string {
	condPrintf(0, "arch:%s:\n", param.impl+ters(param.version != "", "-", "")+param.version)
	return param.impl + ters(param.version != "", "-", "") + param.version +
		ters(!param.archInArchiveName,
			install_impl.extention(param),
			"-"+param.arch+"-"+param.os+install_impl.extention(param))
}

func download(param *installOptions) int {
	url := install_impl.uri(param)
	archive_name := download_archive_name(param)
	impl_archive := configdir() + "archives" + SLASH + archive_name
	condPrintf(0, "impl_archive:%s:\n", impl_archive)
	if !fileExistP(impl_archive) /*|| get_opt("download.force",1) */ {
		condPrintf(0, "Downloading %s\n", url)
		if url != "" {
			ensureDirectoriesExist(impl_archive)
			if downloadSimple(url, impl_archive, verbose)!=0 {
				condPrintf(0,"Failed to Download.\n")
				return 0
			}
		}
	} else {
		condPrintf(0,"Skip downloading %s\n", url)
	}
	return 1
}

func cmdInstallInternal(impl *installImpls, param *installOptions) {
	var ret int
	for _, fun := range impl.call {
		ret = fun(param)
		if ret == 0 {
			break
		}
	}
	if ret == 0 { // after install latest installed impl/version should be default for 'run'
		path := configdir() + "config"
		v := param.impl + ".version"
		version := param.version
		if !impl.util {
			for i := 0; i < len(version); i++ {
				if version[i] == '-' {
					version = version[0:i]
				}
			}
			globalOpt = setOpt(globalOpt, "default.lisp", param.impl, 0)
			globalOpt = setOpt(globalOpt, v, version, 0)
			saveOpts(path, globalOpt)
		}
	} else {
		os.Exit(1)
	}
}

func cmdInstall(argv []string, cmd subCommand) (ret int) {
	//tbd
	var param installOptions
	quicklisp = 1
	param.os = uname()
	param.arch = uname_m()
	param.archInArchiveName = false
	param.expandPath = ""
	//condPrintf(1, "argv:%s:%d\n", argv, len(argv))
	if len(argv) != 1 {
		for k := 1; k < len(argv); k++ {
			param.impl = argv[k]
			if pos := strings.Index(argv[k], "/"); pos != -1 {
				param.version = param.impl[pos+1:]
				param.impl = param.impl[0:pos]
			} else {
				param.version = ""
			}
		}
		for _, impl := range impls_to_install {
			if param.impl == impl.name {
				install_impl = impl
				cmdInstallInternal(impl, &param)
			}
		}
	} else {
		proccmd([]string{"help", "install"}, topOptions, topCommands)
		os.Exit(0)
	}
	return 0
}

func installHelp(argv []string, cmd subCommand) int {
	//dummy
	return 2
}

func registerCmdInternal() {
	topCommands = addCommand(topCommands, "install", "", cmdInstall, 1, true, "Install archive and build it for "+PACKAGE+" environment", "")
	topHelps = addHelp(topHelps, "install", "", nil, nil, "", "", installHelp)
}
