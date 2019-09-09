package gatherlogs

import (
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
