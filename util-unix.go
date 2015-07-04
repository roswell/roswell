// +build !windows

package main

import (
	"strings"
)

func uname() string {
	result, retcode := system_("uname")
	if retcode != 0 {
		return strings.ToLower(strings.Replace(strings.Replace(result, "\n", "", -1), "\r", "", -1))
	} else {
		return ""
	}
}

func uname_m() string {
	result, retcode := system_("uname", "-m")
	if retcode == 0 {
		result = strings.ToLower(strings.Replace(strings.Replace(result, "\n", "", -1), "\r", "", -1))
		switch {
		case result == "i686":
			return "x86"
		case result == "amd64":
			return "x86-64"
		case result == "armv6l" || result == "rmv7l":
			// tbd
			return "armel"
		}
		return strings.Replace(result, "_", "-", -1)
	} else {
		return ""
	}
}
