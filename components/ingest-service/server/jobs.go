package server

import (
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/config"
	log "github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

func (server *JobSchedulerServer) addUpdateJob(jc config.JobConfig) {
	var cronFunc func()

	switch jc.ID {
	case config.DeleteNodes:
		cronFunc = func() {
			deleteExpiredMarkedNodes(context.Background(), jc, server.client)
		}
	case config.NodesMissing:
		cronFunc = func() {
			markNodesMissing(context.Background(), jc, server.client)
		}
	case config.MissingNodesForDeletion:
		cronFunc = func() {
			markMissingNodesForDeletion(context.Background(), jc, server.client)
		}
	default:
		log.Errorf("Job ID %d is not supported", jc.ID)
		return
	}

	server.jobScheduler.AddUpdateJob(jc.JobName(), jc.Threshold, jc.Every, jc.Running, cronFunc)
}

// markNodesMissing is a job that will call the backend to mark
// all nodes that haven't checked in passed the threshold
func markNodesMissing(ctx context.Context, c config.JobConfig, client backend.Client) error {
	log.WithFields(log.Fields{
		"threshold": c.Threshold,
		"job":       c.JobName(),
	}).Debug("Triggering job")

	updateCount, err := client.MarkNodesMissing(ctx, c.Threshold)
	if err != nil {
		log.WithFields(log.Fields{
			"error":     err,
			"threshold": c.Threshold,
			"job":       c.JobName(),
		}).Error("Job error")
		return err
	}

	f := log.Fields{
		"nodes_updated": updateCount,
		"status":        "missing",
		"job":           c.JobName(),
	}
	if updateCount > 0 {
		log.WithFields(f).Info("Job updated nodes")
	} else {
		log.WithFields(f).Debug("Job ran without updates")
	}
	return nil
}

// markMissingNodesForDeletion is a job that will call the backend to mark all missing nodes
// that haven't checked in passed the threshold ready for deletion
func markMissingNodesForDeletion(ctx context.Context, c config.JobConfig, client backend.Client) error {
	log.WithFields(log.Fields{
		"threshold": c.Threshold,
		"job":       c.JobName(),
	}).Debug("Triggering job")

	updateCount, err := client.MarkMissingNodesForDeletion(ctx, c.Threshold)
	if err != nil {
		log.WithFields(log.Fields{
			"error":     err,
			"threshold": c.Threshold,
			"job":       c.JobName(),
		}).Error("Job error")
		return err
	}

	f := log.Fields{
		"nodes_updated": updateCount,
		"exists":        false,
		"job":           c.JobName(),
	}
	if updateCount > 0 {
		log.WithFields(f).Info("Job updated nodes")
	} else {
		log.WithFields(f).Debug("Job ran without updates")
	}
	return nil
}

// deleteExpiredMarkedNodes is a job that will call the backend to delete all expired
// nodes marked for deletion
func deleteExpiredMarkedNodes(ctx context.Context, c config.JobConfig, client backend.Client) error {
	log.WithFields(log.Fields{
		"threshold": c.Threshold,
		"job":       c.JobName(),
	}).Debug("Triggering delete job")

	updateCount, err := client.DeleteMarkedNodes(ctx, c.Threshold)
	if err != nil {
		log.WithFields(log.Fields{
			"error":     err,
			"threshold": c.Threshold,
			"job":       c.JobName(),
		}).Error("Job error")
		return err
	}

	f := log.Fields{
		"nodes_deleted": updateCount,
		"job":           c.JobName(),
	}
	if updateCount > 0 {
		log.WithFields(f).Info("Job deleted nodes")
	} else {
		log.WithFields(f).Debug("Job ran without updates")
	}
	return nil
}
