package server

import (
	"context"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
)

// Watcher is the interface to this component.
type Watcher struct {
	checkInterval time.Duration
	warning       int64
	critical      int64
	esClient      *elastic.Elastic
}

// NewWatcher creates a new Watcher struct using the provided configuration
func NewWatcher(checkInterval time.Duration, warning int64, critical int64, esURL string) (*Watcher, error) {
	esClient, err := elastic.New(esURL)

	if err != nil {
		return nil, errors.Wrap(err, "failed to create elasticsearch client")
	}

	return &Watcher{
		checkInterval: checkInterval,
		warning:       warning,
		critical:      critical,
		esClient:      esClient,
	}, nil
}

// Watch begins polling ES at a configured interval to report on disk usage stats.
// It logs errors when disk space of any mount point on any node falls below the
// warning threshold; and logs errors when it falls below the critical threshold.
func (watcher *Watcher) Watch() {
	log.WithFields(log.Fields{
		"check-interval":     watcher.checkInterval,
		"warning-threshold":  watcher.warning,
		"critical-threshold": watcher.critical,
	}).Info("Starting watcher")

	for {
		perHostDiskStats, err := watcher.esClient.GetDiskStats(context.Background())

		if err != nil {
			log.WithError(err).Error("Could not get disk stats")
		} else {
			success := 0
			warning := 0
			critical := 0

			for _, hostDiskStats := range perHostDiskStats {
				for _, diskStats := range hostDiskStats.PerDiskStats {
					if diskStats.AvailableBytes < watcher.critical {
						log.WithFields(log.Fields{
							"host":            hostDiskStats.Host,
							"mount":           diskStats.MountPoint,
							"avail_bytes":     diskStats.AvailableBytes,
							"total_bytes":     diskStats.TotalBytes,
							"threshold_bytes": watcher.critical,
						}).Error("Disk free below critical threshold")
						critical++
					} else if diskStats.AvailableBytes < watcher.warning {
						log.WithFields(log.Fields{
							"host":            hostDiskStats.Host,
							"mount":           diskStats.MountPoint,
							"avail_bytes":     diskStats.AvailableBytes,
							"total_bytes":     diskStats.TotalBytes,
							"threshold_bytes": watcher.warning,
						}).Warn("Disk free below warning threshold")
						warning++
					} else {
						success++
					}
				}
			}

			if critical+warning == 0 && success > 0 {
				log.Debug("Disk free healthy")
			}
		}

		time.Sleep(watcher.checkInterval)
	}
}
