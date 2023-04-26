package checks

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
)

type Checks struct {
	Logger logger.ILogger
}

type IChecks interface{}

func NewHandler(logger logger.ILogger) IChecks {
	return &Checks{Logger: logger}
}
