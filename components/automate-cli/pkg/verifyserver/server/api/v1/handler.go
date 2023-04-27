package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
)

type Handler struct {
	Logger logger.ILogger
}

func NewHandler(logger logger.ILogger) *Handler {
	return &Handler{Logger: logger}
}
