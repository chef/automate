package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/platform/pg"
)

var cerealCmdFlags = struct {
	DatabaseName string
}{}

var listCerealWorkflowInstanceOpts struct {
	IsRunning    string
	WorkflowName string
	InstanceName string
}

func defaultConnURIForCerealDatabase() string {
	if os.Getenv("PG_URL") != "" {
		return os.Getenv("PG_URL")
	}
	connInfo := pg.A2ConnInfo{
		Host:  "localhost",
		Port:  10145,
		User:  "automate",
		Certs: pg.A2SuperuserCerts,
	}

	return connInfo.ConnURI(cerealCmdFlags.DatabaseName)
}

func getCerealBackend() cereal.Driver {
	return postgres.NewPostgresBackend(defaultConnURIForCerealDatabase())
}

func newCerealCmd() *cobra.Command {
	cerealCmd := &cobra.Command{
		Use:   "cereal",
		Short: "cereal dev helpers",
	}
	cerealCmd.PersistentFlags().StringVar(&cerealCmdFlags.DatabaseName, "database", "chef_cereal_service", "the name of the database to connect to")

	workflowCmd := &cobra.Command{
		Use:   "workflow",
		Short: "workflow helpers",
	}
	cerealCmd.AddCommand(workflowCmd)

	listWorkflowInstancesCmd := &cobra.Command{
		Use:   "list",
		Short: "list the workflow instances",
		RunE:  runCerealListWorkflowInstances,
	}

	listWorkflowInstancesCmd.PersistentFlags().StringVar(&listCerealWorkflowInstanceOpts.IsRunning, "is-running", "", "true or false")
	listWorkflowInstancesCmd.PersistentFlags().StringVar(&listCerealWorkflowInstanceOpts.WorkflowName, "workflow-name", "", "the name of the workflow")
	listWorkflowInstancesCmd.PersistentFlags().StringVar(&listCerealWorkflowInstanceOpts.InstanceName, "instance-name", "", "the name of the instance")
	workflowCmd.AddCommand(listWorkflowInstancesCmd)

	cancelWorkflowInstancesCmd := &cobra.Command{
		Use:   "cancel workflow_name instance_name",
		Short: "cancel the workflow instance",
		Args:  cobra.ExactArgs(2),
		RunE:  runCerealCancelWorkflowInstance,
	}
	workflowCmd.AddCommand(cancelWorkflowInstancesCmd)

	killWorkflowInstancesCmd := &cobra.Command{
		Use:   "kill workflow_name instance_name",
		Short: "kill the workflow instance",
		Args:  cobra.ExactArgs(2),
		RunE:  runCerealKillWorkflowInstance,
	}
	workflowCmd.AddCommand(killWorkflowInstancesCmd)

	schedulesCmd := &cobra.Command{
		Use:   "schedule",
		Short: "scheduler helpers",
	}
	cerealCmd.AddCommand(schedulesCmd)

	listSchedulesCmd := &cobra.Command{
		Use:   "list",
		Short: "list the schedules",
		RunE:  runCerealListSchedules,
	}
	schedulesCmd.AddCommand(listSchedulesCmd)

	triggerScheduleCmd := &cobra.Command{
		Use:   "trigger workflow_name instance_name",
		Short: "trigger the schedules",
		RunE:  runCerealTriggerSchedule,
	}
	schedulesCmd.AddCommand(triggerScheduleCmd)

	return cerealCmd
}

func runCerealListWorkflowInstances(cmd *cobra.Command, args []string) error {
	b := getCerealBackend()
	if err := b.Init(); err != nil {
		return err
	}

	opts := cereal.ListWorkflowOpts{}
	if listCerealWorkflowInstanceOpts.IsRunning == "true" {
		m := true
		opts.IsRunning = &m
	} else if listCerealWorkflowInstanceOpts.IsRunning == "false" {
		m := false
		opts.IsRunning = &m
	}

	if listCerealWorkflowInstanceOpts.InstanceName != "" {
		opts.InstanceName = &listCerealWorkflowInstanceOpts.InstanceName
	}

	if listCerealWorkflowInstanceOpts.WorkflowName != "" {
		opts.WorkflowName = &listCerealWorkflowInstanceOpts.WorkflowName
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

func runCerealCancelWorkflowInstance(cmd *cobra.Command, args []string) error {
	b := getCerealBackend()
	if err := b.Init(); err != nil {
		return err
	}
	return b.CancelWorkflow(context.Background(), args[1], args[0])
}

func runCerealKillWorkflowInstance(cmd *cobra.Command, args []string) error {
	b := getCerealBackend()
	if err := b.Init(); err != nil {
		return err
	}
	return b.KillWorkflow(context.Background(), args[1], args[0])
}

func runCerealListSchedules(cmd *cobra.Command, args []string) error {
	b := getCerealBackend()
	if err := b.Init(); err != nil {
		return err
	}
	schedules, err := b.ListWorkflowSchedules(context.Background())
	if err != nil {
		return err
	}

	for _, s := range schedules {
		fmt.Printf("%13s: %s\n", "Workflow Name", s.WorkflowName)
		fmt.Printf("%13s: %s\n", "Instance Name", s.InstanceName)
		fmt.Printf("%13s: %t\n", "Enabled", s.Enabled)
		fmt.Printf("%13s: %s\n", "Recurrence", s.Recurrence)
		fmt.Printf("%13s: %s\n", "Parameters", string(s.Parameters))
		fmt.Printf("%13s: %s\n", "Next Due", s.NextDueAt.String())
		fmt.Printf("%13s: %s\n", "Last Enqueued", s.LastEnqueuedAt.String())
		fmt.Println("-----------------------------------")
	}
	return nil
}

func runCerealTriggerSchedule(cmd *cobra.Command, args []string) error {
	b := getCerealBackend()
	if err := b.Init(); err != nil {
		return err
	}
	workflowName := args[0]
	instanceName := args[1]

	sched, err := b.GetWorkflowScheduleByName(context.Background(), instanceName, workflowName)
	if err != nil {
		return err
	}

	err = b.UpdateWorkflowScheduleByName(context.Background(), instanceName, workflowName, cereal.WorkflowScheduleUpdateOptions{
		UpdateRecurrence: true,
		Recurrence:       sched.Recurrence,
		NextRunAt:        time.Now(),
	})
	if err != nil {
		return err
	}

	return nil
}
