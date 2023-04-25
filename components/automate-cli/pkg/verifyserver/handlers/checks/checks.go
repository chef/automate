package checks

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
)

type Checks struct {
	Logger logger.ILogger
}

type IChecks interface {
}

func NewHandler(Logger logger.ILogger) *Checks {
	return &Checks{Logger: Logger}
}
