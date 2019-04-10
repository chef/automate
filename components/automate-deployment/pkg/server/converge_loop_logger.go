package server

import (
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

type convergeLoopLogger struct {
}

func newConvergeLoopLogger() converge.EventSink {
	return &convergeLoopLogger{}
}

func (*convergeLoopLogger) Sink(e converge.Event) {
	switch ev := e.(type) {
	case converge.SupervisorUpgradeFinishEvent:
		logctx := logrus.WithField("event_type", ev.Name())
		if ev.Error() != nil {
			logctx.WithError(ev.Error()).Error("Failed to update supervisor")
		} else {
			if ev.Modified() {
				logctx.WithFields(logrus.Fields{
					"modified": ev.Modified(),
				}).Info("Completed converge step")
			} else {
				logctx.WithFields(logrus.Fields{
					"modified": ev.Modified(),
				}).Debug("Completed converge step")
			}
		}
	case converge.ServiceFinishEvent:
		pkg := ev.Pkg()
		logctx := logrus.WithFields(logrus.Fields{
			"pkg":        habpkg.Ident(pkg),
			"event_type": ev.Name(),
		})

		if ev.Error() != nil {
			logctx.WithError(ev.Error()).Error("Failed to converge service")
		} else {
			if ev.Modified() {
				logctx.WithFields(logrus.Fields{
					"modified": ev.Modified(),
				}).Info("Completed converge step")
			} else {
				logctx.WithFields(logrus.Fields{
					"modified": ev.Modified(),
				}).Debug("Completed converge step")
			}
		}
	}
}
