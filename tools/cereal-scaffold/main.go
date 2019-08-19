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
	"github.com/teambition/rrule-go"
	"google.golang.org/grpc"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/platform/pg"
)

var opts struct {
	Debug    bool
	Endpoint string
}

var simpleWorkflowOpts struct {
	DequeueWorkerCount int
	TaskCount          int
	SlowTasks          bool
	NoEnqueue          bool
}

var scheduleOpts struct {
	Name string
}

var listInstanceOpts struct {
	IsRunning    string
	WorkflowName string
	InstanceName string
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
	cmd.PersistentFlags().StringVarP(
		&opts.Endpoint,
		"endpoint",
		"e",
		"",
		"grpc endpoint")

	simpleWorkflowCmd := &cobra.Command{
		Use:           "simple-workflow-test",
		Short:         "Run a workflow that enqueue's no-op tasks",
		SilenceUsage:  true,
		SilenceErrors: true,
		RunE:          runSimpleWorkflow,
	}

	simpleWorkflowCmd.PersistentFlags().IntVar(
		&simpleWorkflowOpts.DequeueWorkerCount,
		"dequeue-worker-count",
		10,
		"Number of workers to dequeue tasks")

	simpleWorkflowCmd.PersistentFlags().BoolVar(
		&simpleWorkflowOpts.NoEnqueue,
		"no-enqueue",
		false,
		"Whether to skip the enqueue")

	simpleWorkflowCmd.PersistentFlags().BoolVar(
		&simpleWorkflowOpts.SlowTasks,
		"slow-tasks",
		false,
		"If true, tasks sleep for 400 seconds",
	)

	simpleWorkflowCmd.PersistentFlags().IntVar(
		&simpleWorkflowOpts.TaskCount,
		"task-count",
		10000,
		"Number of tasks to enqueue")

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

	scheduleCmd.PersistentFlags().StringVar(
		&scheduleOpts.Name,
		"schedule-name",
		"every minute",
		"Name to use for the scheduled workflow",
	)

	listInstancesCmd := &cobra.Command{
		Use:  "list-instances",
		RunE: runListInstances,
	}

	listInstancesCmd.PersistentFlags().StringVar(&listInstanceOpts.IsRunning, "is-running", "", "true or false")
	listInstancesCmd.PersistentFlags().StringVar(&listInstanceOpts.WorkflowName, "workflow-name", "", "the name of the workflow")
	listInstancesCmd.PersistentFlags().StringVar(&listInstanceOpts.InstanceName, "instance-name", "", "the name of the instance")

	cmd.AddCommand(simpleWorkflowCmd)
	cmd.AddCommand(resetDBCmd)
	cmd.AddCommand(scheduleCmd)
	cmd.AddCommand(listInstancesCmd)

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

const defaultDatabaseName = "workflow"

func defaultConnURIForDatabase(dbname string) string {
	if os.Getenv("PG_URI") != "" {
		return os.Getenv("PG_URI")
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

type SimpleTask struct{}

type SimpleTaskParams struct {
	ID     string
	Sleepy int
}

func (t *SimpleTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := SimpleTaskParams{}
	logrus.Debug("here")
	if err := task.GetParameters(&params); err != nil {
		panic(err)
	}
	logrus.WithField("id", params.ID).Debug("Running task")
	if simpleWorkflowOpts.SlowTasks {
		select {
		case <-time.After(time.Duration(23+params.Sleepy) * time.Second):
		case <-ctx.Done():
			logrus.Info("task cancelled")
			return nil, ctx.Err()
		}
	}
	logrus.Debug("Finished Task")
	return params.ID, nil
}

type SimpleWorkflowParams struct {
	NumTasks int
}

type SimpleWorkflow struct{}

func (p *SimpleWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	logrus.Info("SimpleWorkflow got OnStart")

	params := SimpleWorkflowParams{}
	err := w.GetParameters(&params)
	if err != nil {
		panic(err)
	}
	if params.NumTasks == 0 {
		logrus.Error("Got no tasks to do")
		return w.Complete()
	}

	for i := 0; i < params.NumTasks; i++ {
		w.EnqueueTask("simple task", &SimpleTaskParams{ID: fmt.Sprintf("asdf: %d", i), Sleepy: i * 2})
	}

	initialVal := 0
	return w.Continue(&initialVal)
}

var done = false

func (p *SimpleWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {
	var mycount int

	if err := ev.Result.Err(); err != nil {
		logrus.WithError(err).Error("task failed")
	}

	if err := w.GetPayload(&mycount); err != nil {
		logrus.WithError(err).Fatal("Could not decode payload")
	}

	params := SimpleWorkflowParams{}
	if err := w.GetParameters(&params); err != nil {
		logrus.WithError(err).Fatal("Could not decode parameters")
	}

	taskParams := SimpleWorkflowParams{}
	if err := ev.Result.GetParameters(&taskParams); err != nil {
		logrus.WithError(err).Fatal("Could not decode task params in result")
	}

	taskResult := ""
	if err := ev.Result.Get(&taskResult); err != nil {
		logrus.WithError(err).Error("Could not decode task params in result")
	}

	logrus.WithFields(logrus.Fields{
		"task_name":  ev.TaskName,
		"enqueued":   w.TotalEnqueuedTasks(),
		"completed":  w.TotalCompletedTasks(),
		"payload":    mycount,
		"params":     params,
		"taskParams": taskParams,
		"taskResult": taskResult,
	}).Debug("SimpleWorkflow got Task Completed")

	completed := mycount + 1
	if completed < params.NumTasks {
		return w.Continue(&completed)
	} else {
		logrus.Info("SimpleWorkflow marking itself as complete")
		go func() {
			// TODO: This done field is a temporary hack until we have
			// an API for polling the status of a workflow.
			time.Sleep(2 * time.Second)
			done = true
		}()
		return w.Complete()
	}
}

func (SimpleWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

func getBackend(dbName string) backend.Driver {
	if opts.Endpoint != "" {
		conn, err := grpc.Dial(opts.Endpoint, grpc.WithInsecure(), grpc.WithMaxMsgSize(64*1024*1024))
		if err != nil {
			panic(err)
		}
		grpcBackend := grpccereal.NewGrpcBackendFromConn("test", conn)
		return grpcBackend
	}
	return postgres.NewPostgresBackend(defaultConnURIForDatabase(dbName))
}

func runSimpleWorkflow(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	b := getBackend(dbName)
	manager, err := cereal.NewManager(b)
	if err != nil {
		return err
	}
	defer manager.Stop()

	/*
		executor := multiworkflow.NewMultiWorkflowExecutor(map[string]cereal.WorkflowExecutor{
			"supersimple1": &SimpleWorkflow{},
			"supersimple2": &SimpleWorkflow{},
		})
	*/
	executor, err := patterns.NewChainWorkflowExecutor([]cereal.WorkflowExecutor{&SimpleWorkflow{}, &SimpleWorkflow{}})
	if err != nil {
		panic(err)
	}
	//manager.RegisterWorkflowExecutor("simple-workflow", &SimpleWorkflow{})
	manager.RegisterWorkflowExecutor("simple-workflow", executor)
	manager.RegisterTaskExecutor("simple task", &SimpleTask{}, cereal.TaskExecutorOpts{
		Workers: simpleWorkflowOpts.DequeueWorkerCount})

	params := SimpleWorkflowParams{
		simpleWorkflowOpts.TaskCount,
	}
	params2 := SimpleWorkflowParams{
		simpleWorkflowOpts.TaskCount + 10,
	}

	manager.Start(context.Background())

	if !simpleWorkflowOpts.NoEnqueue {
		instanceName := fmt.Sprintf("simple-workflow-%s", time.Now())
		/*
			err = manager.EnqueueWorkflow(context.TODO(),
				"simple-workflow", instanceName,
				&params,
			)*/
		/*
			err = multiworkflow.EnqueueWorkflow(context.TODO(), manager, "simple-workflow", instanceName, map[string]interface{}{
				"supersimple1": params,
				"supersimple2": params2,
			})
			if err != nil {
				logrus.WithError(err).Error("Unexpected error enqueueing workflow")
				return err
			}
		*/
		err = patterns.EnqueueChainWorkflow(context.TODO(), manager, "simple-workflow", instanceName, []interface{}{params, params2})
		if err != nil {
			logrus.WithError(err).Error("Unexpected error enqueueing workflow")
			return err
		}
	}

	for !done {
		time.Sleep(time.Second)
	}

	for {
		time.Sleep(time.Minute)
	}
	return nil
}

type ScheduleTestTask struct{}

func (t *ScheduleTestTask) Run(ctx context.Context, _ cereal.Task) (interface{}, error) {
	logrus.Info("Running schedule test task")
	return nil, nil
}

type ScheduleTestWorkflow struct{}

func (p *ScheduleTestWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {
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

func (p *ScheduleTestWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	logrus.WithFields(logrus.Fields{
		"task_name": ev.TaskName,
		"enqueued":  w.TotalEnqueuedTasks(),
		"completed": w.TotalCompletedTasks(),
	}).Info("ScheduleTestWorkflow got Task Completed")
	return w.Complete()
}

func (p *ScheduleTestWorkflow) OnCancel(w cereal.WorkflowInstance,
	ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

func runScheduleTest(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	b := getBackend(dbName)
	manager, err := cereal.NewManager(b)
	if err != nil {
		return err
	}
	defer manager.Stop()

	manager.RegisterWorkflowExecutor("schedule-test", &ScheduleTestWorkflow{})
	manager.RegisterTaskExecutor("test task", &ScheduleTestTask{}, cereal.TaskExecutorOpts{
		Workers: simpleWorkflowOpts.DequeueWorkerCount,
	})

	recRule, err := rrule.NewRRule(rrule.ROption{
		Freq:    rrule.MINUTELY,
		Dtstart: time.Now().AddDate(0, 0, -1).Add(-55 * time.Second), // This will be due in 5 seconds
	})
	if err != nil {
		panic(err)
	}

	err = manager.CreateWorkflowSchedule(
		scheduleOpts.Name, "schedule-test", "youfail", true, recRule)
	if err != nil {
		if err == cereal.ErrWorkflowScheduleExists {
			logrus.Info("workflow schedule exists...ignoring")
		} else {
			logrus.WithError(err).Warn("could not create workflow schedule")
		}
	}

	schedules, err := manager.ListWorkflowSchedules(context.Background())
	if err != nil {
		logrus.WithError(err).Error("Failed to list workflow schedules")
	}
	for _, s := range schedules {
		logrus.WithField("sched", s).Info("Found schedule")
	}

	manager.UpdateWorkflowScheduleByName(context.Background(),
		schedules[0].InstanceName, schedules[0].WorkflowName, cereal.UpdateParameters("youwin"))

	manager.Start(context.Background())

	for {
		schedules, err := manager.ListWorkflowSchedules(context.Background())
		if err != nil {
			logrus.WithError(err).Error("Failed to list workflow schedules")
		}
		for _, s := range schedules {
			logrus.WithFields(logrus.Fields{
				"name":          s.InstanceName,
				"workflow_name": s.WorkflowName,
				"enabled":       s.Enabled,
				"next_due_at":   s.NextDueAt,
				"last_start":    s.LastStart,
				"last_end":      s.LastEnd,
			}).Debug("Found schedule")
		}
		time.Sleep(10 * time.Second)
	}

	return nil
}

func runListInstances(_ *cobra.Command, args []string) error {
	dbName := defaultDatabaseName
	if len(args) > 0 {
		dbName = args[0]
	}

	b := getBackend(dbName)
	if err := b.Init(); err != nil {
		return err
	}

	opts := backend.ListWorkflowOpts{}
	if listInstanceOpts.IsRunning == "true" {
		m := true
		opts.IsRunning = &m
	} else if listInstanceOpts.IsRunning == "false" {
		m := false
		opts.IsRunning = &m
	}

	if listInstanceOpts.InstanceName != "" {
		opts.InstanceName = &listInstanceOpts.InstanceName
	}

	if listInstanceOpts.WorkflowName != "" {
		opts.WorkflowName = &listInstanceOpts.WorkflowName
	}

	instances, err := b.ListWorkflowInstances(context.Background(), opts)
	if err != nil {
		return err
	}
	for _, instance := range instances {
		fmt.Printf("%13s: %s\n", "Workflow Name", instance.WorkflowName)
		fmt.Printf("%13s: %s\n", "Instance Name", instance.InstanceName)
		fmt.Printf("%13s: %s\n", "Status", string(instance.Status))
		fmt.Printf("%13s: %s\n", "Parameters", string(instance.Parameters))
		fmt.Printf("%13s: %s\n", "Payload", string(instance.Payload))
		fmt.Printf("%13s: %s\n", "Result", string(instance.Result))
		fmt.Printf("%13s: %v\n", "Err", instance.Err)
		fmt.Println("-----------------------------------")
	}

	return nil
}
