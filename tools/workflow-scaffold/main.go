package main

import (
	"context"
	"database/sql"
	"fmt"
	"os"
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
	EnqueueOnly        bool
	DequeueOnly        bool
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
		&perfTestOpts.DequeueWorkerCount,
		"dequeue-worker-count",
		10,
		"Number of workers to dequeue tasks")

	perfTestCmd.PersistentFlags().BoolVar(
		&perfTestOpts.EnqueueOnly,
		"enqueue-only",
		false,
		"Whether to only run the enqueing test")

	perfTestCmd.PersistentFlags().BoolVar(
		&perfTestOpts.DequeueOnly,
		"dequeue-only",
		false,
		"Whether to only run the de-enqueing test (requests a full queue)")

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
	if os.Getenv("JAYM") != "" {
		return fmt.Sprintf("postgresql://docker:docker@127.0.0.1:5432/%s?sslmode=disable", dbname)
	}
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

type PerfTestTask struct {
	statusChan chan struct{}
}

func (t *PerfTestTask) Run(ctx context.Context, _ interface{}) (interface{}, error) {
	t.statusChan <- struct{}{}
	return nil, nil
}

type PerfTestWorkflow struct {
	// NOTE(ssd) 2019-05-10: Storing state in this way is super
	// not what users will do. We need to get payloads &
	// parameters working to remove these.
	total int
	count int
}

func (p *PerfTestWorkflow) OnStart(w workflow.FWorkflowInstance,
	ev workflow.StartEvent) workflow.Decision {
	logrus.Info("PerfTestWorkflow got OnStart")
	for i := 0; i < p.total; i++ {
		w.EnqueueTask("test task", fmt.Sprintf("asdf: %d", i))
	}
	enqueue_done = true
	return w.Continue(0)
}

var enqueue_done = false
var done = false

func (p *PerfTestWorkflow) OnTaskComplete(w workflow.FWorkflowInstance,
	ev workflow.TaskCompleteEvent) workflow.Decision {

	logrus.WithField("task_name", ev.TaskName).Info("PerfTestWorkflow got Task Completed")
	p.count++
	if p.count < p.total {
		return w.Continue(p.count)
	} else {
		logrus.Info("PerfTestWorkflow marking itself as complete")
		go func() {
			time.Sleep(2 * time.Second)
			done = true
		}()
		return w.Complete()
	}
}

func (PerfTestWorkflow) OnCancel(w workflow.FWorkflowInstance,
	ev workflow.CancelEvent) workflow.Decision {
	return w.Complete()
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

	workflowManager := workflow.NewManager(w)
	workflowManager.RegisterWorkflowExecutor("perf-test", &PerfTestWorkflow{
		total: perfTestOpts.TaskCount,
	})
	statusChan := make(chan struct{})
	if !perfTestOpts.EnqueueOnly {
		workflowManager.RegisterTaskExecutor("test task", &PerfTestTask{statusChan}, workflow.TaskExecutorOpts{
			Workers: perfTestOpts.DequeueWorkerCount,
		})
	}

	workflowManager.Start(context.Background())
	w.EnqueueWorkflow(context.TODO(), &workflow.WorkflowInstance{
		WorkflowName: "perf-test",
		InstanceName: fmt.Sprintf("perf-test-%s", time.Now()),
	})

	if perfTestOpts.EnqueueOnly {
		for !enqueue_done {
			time.Sleep(1 * time.Second)
		}
		return nil
	}

	startTime := time.Now()
	dequeueTotal := 0
	for {
		<-statusChan
		dequeueTotal++
		if (dequeueTotal % 1000) == 0 {
			logrus.Infof("dequeue status -- %d in %f seconds",
				dequeueTotal, time.Since(startTime).Seconds())
		}

		if dequeueTotal == perfTestOpts.TaskCount {
			logrus.Infof("All %d tasks enqueued/dequeued in %f seconds, exiting", dequeueTotal, time.Since(startTime).Seconds())
			break
		}
	}

	for !done {
		time.Sleep(1 * time.Second)
	}
	return nil
}
