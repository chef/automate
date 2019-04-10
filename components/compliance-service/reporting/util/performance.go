package util

import (
	"time"

	"github.com/sirupsen/logrus"
)

// TimeTrack logs out time between `start` and current time
func TimeTrack(start time.Time, name string) {
	elapsed := time.Since(start).Truncate(time.Millisecond)
	logrus.Debugf("TimeTrack: %s took %s", name, elapsed)
}
