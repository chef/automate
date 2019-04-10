package clirequest

import "os/exec"

// Cmd returns a exec.Cmd that reruns the current CLI
func Cmd(argv ...string) *exec.Cmd {
	cmd := exec.Command("/proc/self/exe", argv...)
	return cmd
}
