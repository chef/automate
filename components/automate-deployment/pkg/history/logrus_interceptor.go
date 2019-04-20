package history

import (
	"github.com/sirupsen/logrus"
)

type logrusInterceptor struct{}

func NewLogrusInterceptor() EventInterceptor {
	return logrusInterceptor{}
}

func (logrusInterceptor) LogStart(ev *StartEvent) {
	logrus.WithFields(logrus.Fields{
		"id":          ev.ID,
		"class":       ev.Name,
		"description": ev.Description,
		"cause":       ev.Cause,
		"tags":        ev.Tags,
	}).Info("Event Start")
}

func (logrusInterceptor) LogComplete(ev *CompleteEvent) {
	var status string
	if ev.IsSuccess() {
		status = "success"
	} else {
		status = "fail"
	}
	logrus.WithFields(logrus.Fields{
		"id":     ev.ID,
		"status": status,
		"err":    ev.Err,
	}).Info("Event Complete")
}
