package gatherlogs

import (
	"fmt"
	"os"
	"path"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/command"
)

// Copy holds the fetching data
type Copy struct {
	SrcPath  string
	DestPath string
}

func (c *Copy) execute() error {
	copyArgs := command.Args(
		"--recursive",
		"--dereference",
		"--parents",
		c.SrcPath,
		c.DestPath,
	)

	out, err := command.CombinedOutput("cp", copyArgs)
	if err != nil {
		log.WithFields(
			log.Fields{
				"error":  err,
				"path":   c.SrcPath,
				"output": out,
			},
		).Info("Failed to copy file")

		return err
	}

	return nil
}

type Tail struct {
	SrcPath  string
	DestPath string
	Lines    uint64
}

func (c *Tail) execute() error {
	if _, err := os.Stat(c.SrcPath); err != nil {
		if err == os.ErrNotExist {
			return nil
		}
		return err
	}

	dname := path.Dir(c.DestPath)
	if err := os.MkdirAll(dname, 0600); err != nil {
		return err
	}

	lines := c.Lines
	if lines <= 0 {
		lines = 1000
	}
	tailArgs := command.Args(
		"-n",
		fmt.Sprintf("%d", lines),
		c.SrcPath,
	)

	out, err := os.OpenFile(c.DestPath, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		log.WithError(err).WithFields(
			log.Fields{
				"path":   c.SrcPath,
				"output": c.DestPath,
			},
		).Error("Failed to tail file")
		return err
	}
	defer out.Close() // nolint: errcheck

	err = command.Run(
		"tail",
		tailArgs,
		command.Stdout(out),
		command.Stderr(out),
	)
	if err != nil {
		log.WithError(err).WithFields(
			log.Fields{
				"path":   c.SrcPath,
				"output": c.DestPath,
			},
		).Error("Failed to tail file")

		return err
	}

	return nil
}
