package checks

import "github.com/sirupsen/logrus"

type Checks struct {
	Logger *logrus.Logger
}

func NewHandler(log *logrus.Logger) *Checks {
	return &Checks{Logger: log}
}
