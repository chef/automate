package publisher

import (
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

// BuildChefRun Builds the publishers
//func BuildChefRunDateInfo(client backend.Client, numberOfPublishers int) message.ChefRunPipe {
//	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
//		out := make(chan message.ChefRun, 100)
//
//		for i := 0; i < numberOfPublishers; i++ {
//			go ChefRunDateInfo(in, client, out, i)
//		}
//
//		return out
//	}
//}

// ChefRunDateInfo The Chef run publisher pipe. This stores the chef run date information in the datastore.
func ChefRunDateInfo(in <-chan message.ChefRun, client backend.Client, out chan<- message.ChefRun, number int) {
	for msg := range in {
		var megaErr error

		log.WithFields(log.Fields{
			"publisher_id": number,
			"message_id":   msg.ID,
			"buffer_size":  len(out),
		}).Info("Publishing ChefRun")

		runInfo := insertNodeInfo(msg, client)
		errc := merge(runInfo)

		for err := range errc {
			if err != nil {
				if megaErr != nil {
					megaErr = fmt.Errorf(err.Error() + " " + megaErr.Error())
				} else {
					megaErr = err
				}
			}
		}
		if megaErr != nil {
			msg.FinishProcessing(status.Errorf(codes.Internal, megaErr.Error()))
		} else {
			out <- msg
		}
	}
	close(out)
}

func insertNodeInfo(msg message.ChefRun, client backend.Client) <-chan error {
	out := make(chan error)
	go func() {
		log.WithFields(log.Fields{
			"entity_uuid": msg.Run.EntityUuid,
		}).Info("IngestingNode Run Date Info")
		// Ingest NodeState
		err := client.InsertNodeRunDateInfo(msg.Ctx, msg.NodeRun)
		if err != nil {
			log.Errorf("Error inserting Node run date object: %s", err)
			out <- err
		} else {
			out <- nil
		}
		close(out)
	}()
	return out
}
