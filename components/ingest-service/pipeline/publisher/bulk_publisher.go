package publisher

import (
	"context"
	"time"

	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

func BuildBulkRunPublisher(client backend.Client) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		return bulkRunPublisherBundler(in, client)
	}
}

func BuildBulkActionPublisher(client backend.Client) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		return bulkActionPublisherBundler(in, client)
	}
}

func bulkRunPublisherBundler(in <-chan message.ChefRun, client backend.Client) <-chan message.ChefRun {
	maxNumberOfBundledMsgs := 10000
	out := make(chan message.ChefRun, maxNumberOfBundledMsgs)
	go func() {
		bundledMsgs := []message.ChefRun{}
		for msg := range in {
			// Add the message to the bundle
			bundledMsgs = append(bundledMsgs, msg)

			// Only publish the collection of bundled messages if
			// the inbox is empty or there are over 10,000 messages bundled
			// else collect the next message
			if len(in) == 0 || len(bundledMsgs) > maxNumberOfBundledMsgs {
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
					}).Error("bulkRunPublisherBundler Failed")
				} else {
					// elasticsearch publish was successful
					// Send all the bundled messages to the next processor in the pipeline
					for _, publishedMsg := range bundledMsgs {
						out <- publishedMsg
					}
					bundledMsgs = []message.ChefRun{}
					dur := time.Since(start)
					log.WithFields(log.Fields{
						"message":           "ChefRun",
						"publish_time":      dur,
						"bulkRequestsCount": len(bulkableRequests),
					}).Debug("bulkRunPublisherBundler")
				}
			}
		}
		close(out)
	}()

	return out
}

func bulkActionPublisherBundler(in <-chan message.ChefAction, client backend.Client) <-chan message.ChefAction {
	maxNumberOfBundledMsgs := 10000
	out := make(chan message.ChefAction, maxNumberOfBundledMsgs)
	go func() {
		bundledMsgs := []message.ChefAction{}
		for msg := range in {
			// Add the message to the bundle
			bundledMsgs = append(bundledMsgs, msg)

			// Only publish the collection of bundled messages if
			// the inbox is empty or there are over 10,000 messages bundled
			// else collect the next message
			if len(in) == 0 || len(bundledMsgs) > maxNumberOfBundledMsgs {
				start := time.Now()
				bulkableRequests := collectBulkActionRequests(bundledMsgs)
				err := client.SendBulkRequest(context.Background(), bulkableRequests)
				if err != nil {
					// if the publish to elasticsearch fails
					// all the bundled messages failed
					// TODO: we could retry publishing to elasticsearch
					for _, publishedMsg := range bundledMsgs {
						publishedMsg.FinishProcessing(err)
					}
					bundledMsgs = []message.ChefAction{}
					dur := time.Since(start)
					log.WithFields(log.Fields{
						"message":           "ChefAction",
						"error":             err,
						"publish_time":      dur,
						"bulkRequestsCount": len(bulkableRequests),
					}).Error("bulkActionPublisherBundler Failed")
				} else {
					// elasticsearch publish was successful
					// Send all the bundled messages to the next processor in the pipeline
					for _, publishedMsg := range bundledMsgs {
						out <- publishedMsg
					}
					bundledMsgs = []message.ChefAction{}
					dur := time.Since(start)
					log.WithFields(log.Fields{
						"message":           "ChefAction",
						"publish_time":      dur,
						"bulkRequestsCount": len(bulkableRequests),
					}).Debug("bulkActionPublisherBundler")
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

func collectBulkActionRequests(bundledMsgs []message.ChefAction) []elastic.BulkableRequest {
	bulkableRequests := make([]elastic.BulkableRequest, 0, len(bundledMsgs))

	for _, waitingMsg := range bundledMsgs {
		bulkableRequests = append(bulkableRequests, waitingMsg.BulkableRequests...)
	}

	return bulkableRequests
}
