package processor

import (
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

const (
	// minDropOnError is the point at which we will drop the
	// entire queue when dropping messages because of an authz
	// error
	minDropOnError = 16
	// maxDropOnError is the maximum number of messages we will
	// drop in response to an authz error.
	maxDropOnError = 1024
)

type msgDropper struct {
	err        error
	msgsToDrop int
}

func (d *msgDropper) QueueError(err error, queueLength int) {
	d.err = err
	d.msgsToDrop = clampedMsgsToDrop(queueLength)
}

func (d *msgDropper) MaybeDrop(msg message.Finishable) bool {
	if d.msgsToDrop > 0 && d.err != nil {
		d.msgsToDrop--
		log.WithFields(log.Fields{
			"message_id":       msg.MessageID(),
			"drops remainging": d.msgsToDrop,
			"error":            d.err,
		}).Debug("BundleProjectTagging - dropping message because of previous error")
		msg.FinishProcessing(d.err)
		return true
	}

	d.msgsToDrop = 0
	d.err = nil
	return false
}

func clampedMsgsToDrop(queueLength int) int {
	// For small queues, just drop everything
	if queueLength <= minDropOnError {
		return queueLength
	}

	toDrop := queueLength / 2
	if toDrop > maxDropOnError {
		return maxDropOnError
	}

	return toDrop
}
