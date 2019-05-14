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
	SlowTasks          bool
	SkipEnqueue        bool
}

func main() {
	cmd := &cobra.Command{
		Use:           "workflow-scaffold",
		Short:         "Simple tool to play with the PG workflow experiment",
		SilenceUsage:  true,
		SilenceErrors: true,
		Args:          cobra.ExactArgs(1),
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			logrus.SetFormatter(&logrus.TextFormatter{
				FullTimestamp: true,
			})
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

	perfTestCmd.PersistentFlags().BoolVar(
		&perfTestOpts.SlowTasks,
		"slow-tasks",
		false,
		"If true, tasks sleep for 400 seconds",
	)

	perfTestCmd.PersistentFlags().BoolVar(
		&perfTestOpts.SkipEnqueue,
		"skip-enqueue",
		false,
		"if true, a workflow will not be enqueued",
	)

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

	scheduleCmd := &cobra.Command{
		Use:           "schedule-test",
		Short:         "Add a few scheduled jobs",
		SilenceUsage:  true,
		SilenceErrors: true,
		RunE:          runScheduleTest,
	}

	cmd.AddCommand(perfTestCmd)
	cmd.AddCommand(resetDBCmd)
	cmd.AddCommand(scheduleCmd)

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

const defaultDatabaseName = "workflow"

func defaultConnURIForDatabase(dbname string) string {
	if os.Getenv("JAYM") != "" {
		return fmt.Sprintf("postgresql://docker:docker@127.0.0.1:10145/%s?sslmode=disable", dbname)
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

type PerfTestTaskParams struct {
	ID     string
	Sleepy int
}

func (t *PerfTestTask) Run(ctx context.Context, task workflow.TaskQuerier) (interface{}, error) {
	params := PerfTestTaskParams{}
	if err := task.GetParameters(&params); err != nil {
		panic(err)
	}
	logrus.WithField("id", params.ID).Info("Running task")
	if perfTestOpts.SlowTasks {
		time.Sleep(time.Duration(23+params.Sleepy) * time.Second)
	}
	t.statusChan <- struct{}{}
	logrus.Info("Finished Task")
	return params.ID, nil
}

type PerfTestWorkflow struct {
}

func (p *PerfTestWorkflow) OnStart(w workflow.WorkflowInstanceHandler,
	ev workflow.StartEvent) workflow.Decision {

	logrus.Info("PerfTestWorkflow got OnStart")

	params := PerfTestWorkflowParams{}
	err := w.GetParameters(&params)
	if err != nil {
		panic(err)
	}
	if params.NumTasks == 0 {
		logrus.Error("Got no tasks to do")
		return w.Complete()
	}

	for i := 0; i < params.NumTasks; i++ {
		w.EnqueueTask("test task", &PerfTestTaskParams{ID: fmt.Sprintf("asdf: %d", i), Sleepy: i * 2})
	}
	go func() {
		time.Sleep(2 * time.Second)
		enqueue_done = true
	}()
	initialVal := 0
	return w.Continue(&initialVal)
}

var enqueue_done = false
var done = false

type PerfTestWorkflowParams struct {
	NumTasks int
}

func (p *PerfTestWorkflow) OnTaskComplete(w workflow.WorkflowInstanceHandler,
	ev workflow.TaskCompleteEvent) workflow.Decision {
	var mycount int

	if err := w.GetPayload(&mycount); err != nil {
		logrus.WithError(err).Fatal("Could not decode payload")
	}

	params := PerfTestWorkflowParams{}
	if err := w.GetParameters(&params); err != nil {
		logrus.WithError(err).Fatal("Could not decode parameters")
	}

	taskParams := PerfTestTaskParams{}
	if err := ev.Result.GetParameters(&taskParams); err != nil {
		logrus.WithError(err).Fatal("Could not decode task params in result")
	}

	taskResult := ""
	if err := ev.Result.Get(&taskResult); err != nil {
		logrus.WithError(err).Fatal("Could not decode task params in result")
	}

	logrus.WithFields(logrus.Fields{
		"task_name":  ev.TaskName,
		"enqueued":   w.TotalEnqueuedTasks(),
		"completed":  w.TotalCompletedTasks(),
		"payload":    mycount,
		"params":     params,
		"taskParams": taskParams,
		"taskResult": taskResult,
	}).Info("PerfTestWorkflow got Task Completed")

	completed := mycount + 1
	if completed < params.NumTasks {
		return w.Continue(&completed)
	} else {
		logrus.Info("PerfTestWorkflow marking itself as complete")
		go func() {
			time.Sleep(2 * time.Second)
			done = true
		}()
		return w.Complete()
	}
}

func (PerfTestWorkflow) OnCancel(w workflow.WorkflowInstanceHandler,
	ev workflow.CancelEvent) workflow.Decision {
	return w.Complete()
}

func runPerfTest(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	w, err := workflow.NewPostgresBackend(defaultConnURIForDatabase(dbName))
	if err != nil {
		return errors.Wrap(err, "could not initialize database connection")
	}

	err = w.Init()
	if err != nil {
		return errors.Wrap(err, "could not initialize database schema")
	}

	workflowManager := workflow.NewManager(w)
	workflowManager.RegisterWorkflowExecutor("perf-test", &PerfTestWorkflow{})
	statusChan := make(chan struct{})
	if !perfTestOpts.EnqueueOnly {
		workflowManager.RegisterTaskExecutor("test task", &PerfTestTask{statusChan}, workflow.TaskExecutorOpts{
			Workers: perfTestOpts.DequeueWorkerCount,
		})
	}

	params := PerfTestWorkflowParams{
		perfTestOpts.TaskCount,
	}

	workflowManager.Start(context.Background())
	if !perfTestOpts.SkipEnqueue {
		instanceName := fmt.Sprintf("perf-test-%s", time.Now())
		workflowManager.EnqueueWorkflow(context.TODO(),
			"perf-test", instanceName,
			&params,
		)
		err := workflowManager.EnqueueWorkflow(context.TODO(),
			"perf-test", instanceName,
			&params,
		)
		if err == workflow.ErrWorkflowInstanceExists {
			logrus.Info("Successfully can't add multiple workflows with the same name")
		} else {
			logrus.WithError(err).Error("Unspected error")
		}
	}

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

type ScheduleTestTask struct{}

func (t *ScheduleTestTask) Run(ctx context.Context, _ workflow.TaskQuerier) (interface{}, error) {
	logrus.Info("Running schedule test task")
	return nil, nil
}

type ScheduleTestWorkflow struct{}

func (p *ScheduleTestWorkflow) OnStart(w workflow.WorkflowInstanceHandler,
	ev workflow.StartEvent) workflow.Decision {
	var params string
	err := w.GetParameters(&params)
	if err != nil {
		logrus.WithError(err).Error("Failed to get parameters")
		w.Complete()
	}
	logrus.WithField("params", params).Info("Doing OnStart")
	w.EnqueueTask("test task", "asdf")
	return w.Continue(0)
}

func (p *ScheduleTestWorkflow) OnTaskComplete(w workflow.WorkflowInstanceHandler,
	ev workflow.TaskCompleteEvent) workflow.Decision {

	logrus.WithFields(logrus.Fields{
		"task_name": ev.TaskName,
		"enqueued":  w.TotalEnqueuedTasks(),
		"completed": w.TotalCompletedTasks(),
	}).Info("ScheduleTestWorkflow got Task Completed")
	return w.Complete()
}

func (p *ScheduleTestWorkflow) OnCancel(w workflow.WorkflowInstanceHandler,
	ev workflow.CancelEvent) workflow.Decision {
	return w.Complete()
}

func runScheduleTest(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	w, err := workflow.NewPostgresBackend(defaultConnURIForDatabase(dbName))
	if err != nil {
		return errors.Wrap(err, "could not initialize database connection")
	}

	err = w.Init()
	if err != nil {
		return errors.Wrap(err, "could not initialize database schema")
	}

	workflowManager := workflow.NewManager(w)
	workflowManager.RegisterWorkflowExecutor("schedule-test", &ScheduleTestWorkflow{})
	workflowManager.RegisterTaskExecutor("test task", &ScheduleTestTask{}, workflow.TaskExecutorOpts{
		Workers: perfTestOpts.DequeueWorkerCount,
	})

	err = workflowManager.CreateWorkflowSchedule(
		"every minute", "schedule-test", "youfail", true, "FREQ=MINUTELY")
	if err != nil {
		if err == workflow.ErrWorkflowScheduleExists {
			logrus.Info("workflow schedule exists...ignoring")
		} else {
			logrus.WithError(err).Warn("could not create workflow schedule")
		}
	}

	schedules, err := w.ListWorkflowSchedules(context.Background())
	if err != nil {
		logrus.WithError(err).Error("Failed to list workflow schedules")
	}
	for _, s := range schedules {
		logrus.WithField("sched", s).Info("Found schedule")
	}

	w.UpdateWorkflowScheduleByID(context.Background(),
		schedules[0].ID, workflow.UpdateParameters("youwin"))

	workflowManager.Start(context.Background())

	for {
		time.Sleep(1 * time.Second)
	}

	return nil
}
