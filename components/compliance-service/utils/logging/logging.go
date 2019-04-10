package logging

import (
	"github.com/sirupsen/logrus"
)

// SetLogLevel sets the logrus logging level
func SetLogLevel(logLevel string) {
	if logLevel != "" {
		lvl, err := logrus.ParseLevel(logLevel)
		if err != nil {
			logrus.Errorf("Unable to parse logging level: %s", logLevel)
			logrus.Error("Acceptable log levels are: debug, info, warning, panic, and fatal.")
		}
		logrus.SetLevel(lvl)
	} else {
		logrus.SetLevel(logrus.InfoLevel)
	}
}
