package main

import (
	"os/exec"
	"syscall"
)

func system_(cmd ...string) (ret string, exitCode int) {
	cm := exec.Command(cmd[0], cmd[1:]...)
	out, err := cm.Output()

	if err != nil {
		if err2, ok := err.(*exec.ExitError); ok {
			if s, ok := err2.Sys().(syscall.WaitStatus); ok {
				err = nil
				exitCode = s.ExitStatus()
			}
		}
	}
	return string(out), exitCode
}
