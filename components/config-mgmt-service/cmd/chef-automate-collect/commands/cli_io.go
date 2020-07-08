package commands

import (
	"fmt"
	"os"
	"strings"
)

var cliIO = &CLIIO{}

type CLIIO struct {
	EnableVerbose bool
}

func (c *CLIIO) msg(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, newlineify(format), args...)
}

func (c *CLIIO) out(format string, args ...interface{}) {
	fmt.Fprintf(os.Stdout, newlineify(format), args...)
}

func (c *CLIIO) verbose(format string, args ...interface{}) {
	if c.EnableVerbose {
		c.msg(newlineify(format), args...)
	}
}

func newlineify(s string) string {
	if !strings.HasSuffix(s, "\n") {
		return fmt.Sprintf("%s\n", s)
	}
	return s
}
