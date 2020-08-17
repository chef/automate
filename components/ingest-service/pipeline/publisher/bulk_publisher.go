package publisher

import (
	"context"
	"fmt"
	"time"

	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

func BuildBulkRunPublisher(client backend.Client, maxNumberOfBundledRunMsgs int) message.ChefRunPipe {
	count := 0
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		name := fmt.Sprintf("pub-%d", count)
		count++
		return bulkRunPublisherBundler(in, client, maxNumberOfBundledRunMsgs, name)
	}
}

func bulkRunPublisherBundler(in <-chan message.ChefRun, client backend.Client,
	maxNumberOfBundledRunMsgs int, name string) <-chan message.ChefRun {
	log.WithFields(log.Fields{
		"maxNumberOfBundledRunMsgs": maxNumberOfBundledRunMsgs,
		"name":                      name,
	}).Debug("starting bulkRunPublisherBundler")
	out := make(chan message.ChefRun, maxNumberOfBundledRunMsgs)
	go func() {
		bundledMsgs := []message.ChefRun{}
		for msg := range in {
			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessing(err)
			} else {
				// Add the message to the bundle
				bundledMsgs = append(bundledMsgs, msg)
			}

			if len(bundledMsgs) == 0 {
				continue
			}

			// Only publish the collection of bundled messages if
			// the inbox is empty or there are over maxNumberOfBundledRunMsgs number of messages bundled
			// else collect the next message
			if len(in) == 0 || len(bundledMsgs) > maxNumberOfBundledRunMsgs {
				start := time.Now()
				bulkableRequests := collectBulkRunRequests(bundledMsgs)
				err := client.SendBulkRequest(context.Background(), bulkableRequests)
				if err != nil {
					// if the publish to elasticsearch fails
					// all the bundled messages failed
					// TODO: we could retry publishing to elasticsearch
					for _, publishedMsg := range bundledMsgs {
						publishedMsg.FinishProcessing(err)
					}
					bundledMsgs = []message.ChefRun{}
					dur := time.Since(start)
					log.WithFields(log.Fields{
						"publish time":      dur,
						"bulkRequestsCount": len(bulkableRequests),
						"error":             err,
						"message":           "ChefRun",
						"name":              name,
					}).Error("bulkRunPublisherBundler Failed")
				} else {
					// elasticsearch publish was successful
					// Send all the bundled messages to the next processor in the pipeline
					for _, publishedMsg := range bundledMsgs {
						message.PropagateChefRun(out, &publishedMsg)
					}
					bundledMsgs = []message.ChefRun{}
					dur := time.Since(start)
					log.WithFields(log.Fields{
						"message":           "ChefRun",
						"publish_time":      dur,
						"name":              name,
						"bulkRequestsCount": len(bulkableRequests),
					}).Debug("bulkRunPublisherBundler")
				}
			}
		}
		close(out)
	}()

	return out
}

func collectBulkRunRequests(bundledMsgs []message.ChefRun) []elastic.BulkableRequest {
	bulkableRequests := make([]elastic.BulkableRequest, 0, len(bundledMsgs)*3)

	for _, waitingMsg := range bundledMsgs {
		bulkableRequests = append(bulkableRequests, waitingMsg.BulkableRequests...)
	}

	return bulkableRequests
}
