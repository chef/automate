package gatherlogs

import (
	"io"
	"strings"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/command"
)

// Command represents a system command
type Command struct {
	Name          string   // friendly name (used for uniq ref, file name, etc)
	Cmd           string   // command to run
	Args          []string // arguments to the command
	OutputHandler func() (io.WriteCloser, error)
}

func (c *Command) execute() error {
	cmdString := c.Cmd + " " + strings.Join(c.Args, " ")
	logctx := log.WithField("command", cmdString)

	out, err := c.OutputHandler()
	if err != nil {
		logctx.WithError(err).Error("failed to open to output file")
		return err
	}
	defer out.Close()

	logctx.Info("Collecting output for support bundle")
	_, err = io.WriteString(out, cmdString+"\n\n")
	if err != nil {
		logctx.WithError(err).Error("failed to write to output file")
		return err
	}

	err = command.Run(c.Cmd,
		command.Args(c.Args...),
		command.Stdout(out),
		command.Stderr(out))
	if err != nil {
		logctx.WithError(err).Info("Failed to execute command")
		return err
	}
	return nil
}
