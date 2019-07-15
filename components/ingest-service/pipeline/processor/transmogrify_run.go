package processor

import (
	"encoding/json"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

// BuildTransmogrify Builds the Transmogrify Processor(s)
func BuildTransmogrify(numProcessors int) message.ChefRunPipe {
	log.WithFields(log.Fields{
		"numProcessors": numProcessors,
	}).Debug("BuildTransmogrify")
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		out := make(chan message.ChefRun, 100)

		for i := 0; i < numProcessors; i++ {
			ChefRunTransmogrify(in, out, i)
		}

		return out
	}
}

func ChefRunTransmogrify(in <-chan message.ChefRun, out chan<- message.ChefRun, number int) {
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"processor_id": number,
				"message_id":   msg.ID,
				"buffer_size":  len(out),
			}).Debug("Transforming ChefRun")

			runJSONBytes, err := msg.Run.ToJSONBytes()
			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to transform Run message to JSON: %v", err)
				msg.FinishProcessing(grpcErr)
				continue
			}

			// TODO @afiune Improve this unmarshaling with a manual conversion! (AIA-457)
			//
			// This could be replaced for:
			//ccr, err := backend.ParseBytesToChefRun(msg.Run)
			var ccr backend.ChefClientRun
			err = json.Unmarshal(runJSONBytes, &ccr)
			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to transform Run message to ChefClientRun: %v", err)
				msg.FinishProcessing(grpcErr)
				continue
			}

			nodeAttribute, err := ccr.ToNodeAttribute()
			if err != nil {
				log.WithFields(log.Fields{
					"error": err.Error(),
				}).Error("Unable to transform ChefClientRun message to NodeAttribute")
			}

			msg.NodeAttribute = nodeAttribute
			msg.Node, err = ccr.ToNode()
			if err != nil {
				msg.FinishProcessing(err)
				continue
			}

			msg.NodeRun, err = ccr.ToNodeRun()
			if err != nil {
				msg.FinishProcessing(err)
				continue
			}

			out <- msg
		}
		close(out)
	}()
}

// ChefRunCorrections - This processor makes updates and corrections to the raw data from the chef run message.
func ChefRunCorrections(in <-chan message.ChefRun) <-chan message.ChefRun {
	out := make(chan message.ChefRun, 100)
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
			}).Debug("Corrections ChefRun")

			cleanUpErrorTitle(&msg)

			out <- msg
		}
		close(out)
	}()

	return out
}

// This method cleans up an error message comming from chef server.
// Because 412 "Precondition Failed" is not user friendly, the message was changed to
// "Error Resolving Cookbooks for Run List."
func cleanUpErrorTitle(chefRun *message.ChefRun) {
	if chefRun.NodeRun.Error.Description.Title == "412 \"Precondition Failed\"" ||
		chefRun.NodeRun.Error.Description.Title == "Error Resolving Cookbooks for Run List:" {
		chefRun.NodeRun.Error.Description.Title = "Error Resolving Cookbooks for Run List."
	}
}
