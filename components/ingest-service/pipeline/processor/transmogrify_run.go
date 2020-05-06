package processor

import (
	"encoding/json"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
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

			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessing(err)
				continue
			}

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

			// Needed for the nodemanager because the platform field is combined with the version.
			msg.Platform = ccr.Platform()

			message.PropagateChefRun(out, &msg)
		}
		close(out)
	}()
}
