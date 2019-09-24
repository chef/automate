package converge

import (
	"os"
	"syscall"

	"github.com/sirupsen/logrus"
)

// SelfHuper sends a HUP to the current process.  This is used for cases
// where we think hab-sup should be reconfiguring us but we don't
// receive a HUP in a timely manner.
type SelfHuper interface {
	Hup()
}

type selfHuper struct{}

// Hup sends the HUP to the calling process
func (s *selfHuper) Hup() {
	myPid := os.Getpid()
	err := syscall.Kill(myPid, syscall.SIGHUP)
	if err != nil {
		logrus.WithError(err).Warnf("Failed to send HUP to self (pid = %d)", myPid)
	}
}
