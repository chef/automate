package commands

import (
	"fmt"
	"os"
)

var cliIO = &CLIIO{}

type CLIIO struct {
	EnableVerbose bool
}

func (c *CLIIO) msg(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
}

func (c *CLIIO) out(format string, args ...interface{}) {
	fmt.Fprintf(os.Stdout, format, args...)
}

func (c *CLIIO) verbose(format string, args ...interface{}) {
	if c.EnableVerbose {
		c.msg(format, args...)
	}
}
