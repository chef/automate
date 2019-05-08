package main

import (
	"context"
	"database/sql"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/workflow"
)

var opts = struct {
	Debug bool
}{}

var perfTestOpts struct {
	EnqueueWorkerCount int
	DequeueWorkerCount int
	TaskCount          int
}

func main() {
	cmd := &cobra.Command{
		Use:           "workflow-scaffold",
		Short:         "Simple tool to play with the PG workflow experiment",
		SilenceUsage:  true,
		SilenceErrors: true,
		Args:          cobra.ExactArgs(1),
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			}
		},
	}
	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	perfTestCmd := &cobra.Command{
		Use:           "perf-test",
		Short:         "Run a simple perfTest",
		SilenceUsage:  true,
		SilenceErrors: true,
		RunE:          runPerfTest,
	}

	perfTestCmd.PersistentFlags().IntVar(
		&perfTestOpts.EnqueueWorkerCount,
		"enqueue-worker-count",
		10,
		"Number of workers to enqueue tasks")

	perfTestCmd.PersistentFlags().IntVar(
		&perfTestOpts.DequeueWorkerCount,
		"dequeue-worker-count",
		10,
		"Number of workers to dequeue tasks")

	perfTestCmd.PersistentFlags().IntVar(
		&perfTestOpts.TaskCount,
		"task-count",
		10000,
		"Number of tasks to enqueue (split across workers)")

	resetDBCmd := &cobra.Command{
		Use:           "reset-db DATABASE",
		Short:         "DROP and CREATE the named DB",
		SilenceUsage:  true,
		SilenceErrors: true,
		RunE:          runResetDB,
	}

	cmd.AddCommand(perfTestCmd)
	cmd.AddCommand(resetDBCmd)

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

const defaultDatabaseName = "workflow"

func defaultConnURIForDatabase(dbname string) string {
	connInfo := pg.A2ConnInfo{
		Host:  "localhost",
		Port:  5432,
		User:  "automate",
		Certs: pg.A2SuperuserCerts,
	}
	return connInfo.ConnURI(dbname)
}

func runResetDB(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	db, err := sql.Open("postgres", defaultConnURIForDatabase("template1"))
	if err != nil {
		return errors.Wrap(err, "could not initialize db connection")
	}
	_, err = db.Exec(pg.DropDatabaseQuery(dbName))
	if err != nil {
		return errors.Wrap(err, "could not drop database")
	}
	_, err = db.Exec(pg.CreateDatabaseQuery(dbName))
	if err != nil {
		return errors.Wrap(err, "could not create database")
	}
	return nil
}

func runPerfTest(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})

	w, err := workflow.NewPostgresBackend(defaultConnURIForDatabase(dbName))
	if err != nil {
		return errors.Wrap(err, "could not initialize database connection")
	}

	err = w.Init()
	if err != nil {
		return errors.Wrap(err, "could not initialize database schema")
	}

	enqueueResultChan := make(chan error, 100)
	dequeueResultChan := make(chan error, 100)
	doneChan := make(chan struct{})

	perWorkerTaskCount := perfTestOpts.TaskCount / perfTestOpts.EnqueueWorkerCount
	for i := 0; i < perfTestOpts.EnqueueWorkerCount; i++ {
		go enqueueWorker(w, i, perWorkerTaskCount, enqueueResultChan)
	}

	for i := 0; i < perfTestOpts.DequeueWorkerCount; i++ {
		go dequeueWorker(w, i, dequeueResultChan, doneChan)
	}

	startTime := time.Now()
	stats := struct {
		enqueueTotal     int
		enqueueSuccesses int
		enqueueErrors    int

		dequeueTotal     int
		dequeueSuccesses int
		dequeueErrors    int
	}{}
	for {
		select {
		case enRes := <-enqueueResultChan:
			stats.enqueueTotal++
			if enRes == nil {
				stats.enqueueSuccesses++
			} else {
				stats.enqueueErrors++
			}
			if ((stats.enqueueSuccesses + stats.enqueueErrors) % 1000) == 0 {
				logrus.Infof("enqueue status -- %d attempts (%d success, %d failures) in %f seconds",
					stats.enqueueTotal, stats.enqueueSuccesses, stats.enqueueErrors, time.Since(startTime).Seconds())
			}
			if (stats.enqueueSuccesses) == perfTestOpts.TaskCount {
				close(doneChan)
			}
		case deRes := <-dequeueResultChan:
			stats.dequeueTotal++
			if deRes == nil {
				stats.dequeueSuccesses++
			} else {
				stats.dequeueErrors++
			}
			if ((stats.dequeueTotal) % 1000) == 0 {
				logrus.Infof("dequeue status -- %d attempts (%d success, %d failures) in %f seconds",
					stats.dequeueTotal, stats.dequeueSuccesses, stats.dequeueErrors, time.Since(startTime).Seconds())
			}
			if (stats.dequeueSuccesses) == perfTestOpts.TaskCount {
				logrus.Infof("All %d tasks enqueued and dequeued in %f seconds, exiting", perfTestOpts.TaskCount, time.Since(startTime).Seconds())
				return nil
			}
		}
	}

	return nil
}

func enqueueWorker(w workflow.Backend, workerID int, count int, enqueueResultChan chan error) {
	logctx := logrus.WithFields(logrus.Fields{
		"total_count": count,
		"worker_id":   workerID,
	})
	logctx.Info("starting enqueue worker")
	for enqueued := 0; enqueued < count; {
		logctx.Debugf("Enqueueing task %d", enqueued)
		err := w.EnqueueTask(context.TODO(), &workflow.Task{
			Name:               "test task",
			WorkflowInstanceID: 1,
			Parameters:         "",
		})
		enqueueResultChan <- err
		if err == nil {
			logctx.Debugf("Enqueued task %d", enqueued)
			enqueued++
		} else {
			logctx.Error(err)
		}
	}
	logctx.Info("enqueue worker done")
}

func dequeueWorker(w workflow.Backend, workerID int, dequeueResultChan chan error, doneChan chan struct{}) {
	logctx := logrus.WithFields(logrus.Fields{
		"worker_id": workerID,
	})
	logctx.Info("starting dequeue worker")
	for {
		_, taskCompleter, err := w.DequeueTask(context.TODO(), "test task")
		if err != nil {
			if err == workflow.ErrNoTasks {
				select {
				case <-doneChan:
					logctx.Info("No tasks available and all tasks have been queued, exiting")
					return
				default:
					logctx.Debug("No tasks available")
				}

			} else {
				logctx.WithError(err).Error("Failed to dequeue task!")
				dequeueResultChan <- err
			}
			continue
		}
		err = taskCompleter.Succeed("")
		if err != nil {
			logctx.WithError(err).Error("failed to complete task")
			dequeueResultChan <- err
			continue
		}
		dequeueResultChan <- nil
	}
}
