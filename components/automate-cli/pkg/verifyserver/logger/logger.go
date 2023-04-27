package logger

import (
	"os"

	"github.com/sirupsen/logrus"
)

type ILogger interface {
	Info(args ...interface{})
	Error(args ...interface{})
	GetLevel() logrus.Level
}

type LogActions struct {
	log logrus.Logger
}

func NewLogger(debug bool) ILogger {
	level := logrus.InfoLevel
	if debug {
		level = logrus.DebugLevel
	}
	lg := &logrus.Logger{
		Out:       os.Stderr,
		Formatter: &logrus.TextFormatter{TimestampFormat: "2006-01-02 15:04:05.000", FullTimestamp: true},
		Hooks:     make(logrus.LevelHooks),
		Level:     level,
	}
	return lg
}

func (la *LogActions) Info(args ...interface{}) {
	la.log.Info()
}

func (la *LogActions) Error(args ...interface{}) {
	la.log.Error()
}

func (la *LogActions) GetLevel() string {
	return la.log.GetLevel().String()
}
