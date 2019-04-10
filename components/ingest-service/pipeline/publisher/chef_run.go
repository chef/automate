package publisher

import (
	"fmt"
	"sync"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

// BuildChefRun Builds the publishers
func BuildChefRun(client backend.Client, numberOfPublishers int) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		out := make(chan message.ChefRun, 100)

		for i := 0; i < numberOfPublishers; i++ {
			ChefRun(in, client, out, i)
		}

		return out
	}
}

// ChefRun The Chef run publisher pipe. This stores the chef run information in the datastore.
func ChefRun(in <-chan message.ChefRun, client backend.Client, out chan<- message.ChefRun, number int) {
	go func() {
		for msg := range in {
			var megaErr error

			log.WithFields(log.Fields{
				"publisher_id": number,
				"message_id":   msg.ID,
				"buffer_size":  len(out),
			}).Debug("Publishing ChefRun")

			runc := insertChefRun(msg, client)
			nodec := insertNode(msg, client)
			attrc := insertNodeAttribute(msg, client)
			errc := merge(runc, nodec, attrc)

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
	}()
}

func merge(cs ...<-chan error) <-chan error {
	var wg sync.WaitGroup
	out := make(chan error)

	// Start an output goroutine for each input channel in cs.  output
	// copies values from c to out until c is closed, then calls wg.Done.
	output := func(c <-chan error) {
		for err := range c {
			out <- err
		}
		wg.Done()
	}
	wg.Add(len(cs))
	for _, c := range cs {
		go output(c)
	}

	// Start a goroutine to close out once all the output goroutines are
	// done.  This must start after the wg.Add call.
	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}

func insertChefRun(msg message.ChefRun, client backend.Client) <-chan error {
	out := make(chan error)
	go func() {
		log.WithFields(log.Fields{
			"run_id": msg.NodeRun.RunID,
		}).Debug("Ingesting Run")

		err := client.InsertRun(msg.Ctx, msg.NodeRun)
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Error("Unable to ingest Run object")
			out <- err
		} else {
			out <- nil
		}
		close(out)
	}()
	return out
}

func insertNode(msg message.ChefRun, client backend.Client) <-chan error {
	out := make(chan error)
	go func() {
		log.WithFields(log.Fields{
			"entity_uuid": msg.NodeRun.EntityUuid,
		}).Debug("Ingesting Node")
		// Ingest NodeState
		err := client.InsertNode(msg.Ctx, msg.Node)
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Error("Unable to ingest Node object")
			out <- err
		} else {
			out <- nil
		}
		close(out)
	}()
	return out
}

func insertNodeAttribute(msg message.ChefRun, client backend.Client) <-chan error {
	out := make(chan error)
	go func() {
		log.WithFields(log.Fields{
			"entity_uuid": msg.NodeRun.EntityUuid,
		}).Debug("Ingesting Node Attribute")
		// Ingest NodeState
		err := client.InsertNodeAttribute(msg.Ctx, msg.NodeAttribute)
		if err != nil {
			log.Errorf("Error inserting Node object: %s", err)
			out <- err
		} else {
			out <- nil
		}
		close(out)
	}()
	return out
}
