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
		"id":     ev.ID,
		"class":  ev.EventClass.Name,
		"cause":  ev.Cause,
		"tags":   ev.Tags,
		"status": "starting",
	}).Info(ev.EventClass.Description)
}

func (logrusInterceptor) LogComplete(ev *CompleteEvent) {
	if ev.IsSuccess() {
		logrus.WithFields(logrus.Fields{
			"id":     ev.ID,
			"class":  ev.EventClass.Name,
			"status": "success",
			"err":    ev.Err,
		}).Info(ev.EventClass.Description)
	} else {
		logrus.WithFields(logrus.Fields{
			"id":     ev.ID,
			"class":  ev.EventClass.Name,
			"status": "fail",
			"err":    ev.Err,
		}).WithError(ev.Err).Error(ev.EventClass.Description)
	}

}
