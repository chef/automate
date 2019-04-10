package processor

import (
	"time"

	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

// StartedAt time will be a constant to know at what time the service was started
var StartedAt = time.Now()

// CountActions
func CountActions(counter *int64) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		out := make(chan message.ChefAction, 100)

		go func() {
			for msg := range in {
				*counter = *counter + 1

				log.WithFields(log.Fields{
					"message_id":         msg.ID,
					"message":            "ChefAction",
					"service_started_at": StartedAt,
					"total_messages":     *counter,
					"buffer_size":        len(out),
				}).Debug("Counting pipeline messages")

				out <- msg
			}
			close(out)
		}()

		return out
	}
}

// CountRuns
func CountRuns(counter *int64) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		out := make(chan message.ChefRun, 100)

		go func() {
			for msg := range in {
				*counter = *counter + 1

				log.WithFields(log.Fields{
					"message_id":         msg.ID,
					"message":            "ChefRun",
					"service_started_at": StartedAt,
					"total_messages":     *counter,
				}).Debug("Counting pipeline messages")

				out <- msg
			}
			close(out)
		}()

		return out
	}
}
