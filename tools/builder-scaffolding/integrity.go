package main

import (
	"bytes"
	"context"
	"encoding/json"
	"os"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/backup"
)

var integrityCmd = &cobra.Command{
	Use: "integrity",
}

var integrityValidateCmd = &cobra.Command{
	Use:  "validate",
	RunE: runIntegrityValidate,
}

var integrityShowCmd = &cobra.Command{
	Use:  "show",
	RunE: runIntegrityShow,
}

func init() {
	integrityCmd.AddCommand(integrityValidateCmd)
	integrityCmd.AddCommand(integrityShowCmd)
}

func runIntegrityValidate(c *cobra.Command, snapshots []string) error {
	ctx := context.Background()
	var opts []backup.FilterSnapshotOpt

	artifactRepo, _ := artifactRepo()
	if len(snapshots) > 0 {
		opts = append(opts, backup.OnlySnapshots(snapshots))
	}

	return artifactRepo.ValidateSnapshotIntegrity(ctx, opts...)
}

func runIntegrityShow(c *cobra.Command, args []string) error {
	ctx := context.Background()
	artifactRepo, _ := artifactRepo()

	integrityMetadata, err := artifactRepo.ReadSnapshotIntegrityMetadata(ctx)
	if err != nil {
		return err
	}

	b, err := json.Marshal(integrityMetadata)
	if err != nil {
		return err
	}

	text := bytes.Buffer{}
	err = json.Indent(&text, b, "=", "\t")
	if err != nil {
		return err
	}

	_, err = text.WriteTo(os.Stdout)
	return err
}
