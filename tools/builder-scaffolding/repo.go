package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"net"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/lib/secrets"
)

var repoCmd = &cobra.Command{
	Use: "repo",
}

var snapshotCmd = &cobra.Command{
	Use:  "snapshot",
	RunE: runSnapshot,
}

var restoreCmd = &cobra.Command{
	Use:  "restore",
	Args: cobra.ExactArgs(1), // nolint: gomnd
	RunE: runRestore,
}

var removeSnapshotCmd = &cobra.Command{
	Use:  "remove",
	Args: cobra.ExactArgs(1),
	RunE: runRemoveSnapshot,
}

func init() {
	repoCmd.AddCommand(snapshotCmd)
	repoCmd.AddCommand(restoreCmd)
	repoCmd.AddCommand(removeSnapshotCmd)
}

type progressReporter struct {
	formatString string
}

func (r *progressReporter) ReportProgress(completed int64, total int64) {
	logrus.Infof(r.formatString, completed, total)
}

func backupLocationSpec(hostname string, rootCert []byte, secretStore secrets.SecretsReader) backup.LocationSpecification {
	spec, err := backup.NewBackupGatewayLocationSpec(
		fmt.Sprintf("https://%s:10143", hostname),
		"backups",
		"",
		rootCert,
		secretStore,
	)
	if err != nil {
		panic(err)
	}
	return spec
}

func bldrLocationSpec(hostname string, rootCert []byte, secretStore secrets.SecretsReader) backup.LocationSpecification {
	spec, err := backup.NewMinioLocationSpec(
		fmt.Sprintf("https://%s:10106", hostname),
		"depot",
		"",
		"minio",
		rootCert,
		secretStore,
	)
	if err != nil {
		panic(err)
	}
	return spec
}

func artifactRepo() (*backup.ArtifactRepo, backup.Bucket) {
	ipaddr, err := outgoingIPWithUDP()
	if err != nil {
		logrus.WithError(err).Fatal("Could not get ip")
	}
	hostname := ipaddr.String()
	rootCert, err := ioutil.ReadFile("/hab/svc/deployment-service/data/root.crt")
	if err != nil {
		panic(err)
	}
	secretStore := secrets.NewDiskStoreReader("/hab/svc/deployment-service/data/shared")

	bldrSpec := bldrLocationSpec(hostname, rootCert, secretStore)
	bldrBucket := bldrSpec.ToBucket("")
	backupSpec := backupLocationSpec(hostname, rootCert, secretStore)

	return backup.NewArtifactRepo(backupSpec), bldrBucket
}

func runSnapshot(c *cobra.Command, args []string) error {
	ctx := context.Background()

	artifactRepo, bldrBucket := artifactRepo()
	objects, _, err := bldrBucket.List(ctx, "", false)
	if err != nil {
		logrus.WithError(err).Fatal("failed to list builder artifacts")
	}

	requiredArtifacts := make([]string, 0, len(objects))

	for _, o := range objects {
		logrus.Infof("Builder Artifact: %s", o.Name)
		requiredArtifacts = append(requiredArtifacts, o.Name)
	}
	requiredArtifactsStream := backup.NewArrayStream(requiredArtifacts)

	snapshotName := time.Now().Format("20060102150405")

	progress := &progressReporter{"uploaded %d of %d"}

	meta, err := artifactRepo.Snapshot(ctx, snapshotName, bldrBucket,
		requiredArtifactsStream, backup.ArtifactRepoSnapshotReportProgress(progress))
	if err != nil {
		logrus.WithError(err).Fatal("Failed to snapshot")
	}

	fmt.Printf("%-20s %s\n", meta.Name, meta.Checksum)
	return nil
}

func runRestore(c *cobra.Command, args []string) error {
	ctx := context.Background()

	artifactRepo, bldrBucket := artifactRepo()
	progress := &progressReporter{"downloaded %d of %d"}

	if err := artifactRepo.Restore(ctx, bldrBucket, args[0],
		backup.ArtifactRepoRestoreReportProgress(progress)); err != nil {
		logrus.WithError(err).Fatal("Failed to restore snapshot")
	}

	return nil
}

func runRemoveSnapshot(c *cobra.Command, args []string) error {
	ctx := context.Background()

	artifactRepo, _ := artifactRepo()

	if err := artifactRepo.Remove(ctx, args[0]); err != nil {
		logrus.WithError(err).Fatal("failed to remove snapshot")
	}
	return nil
}

func outgoingIPWithUDP() (net.IP, error) {
	c, err := net.Dial("udp", "8.8.8.8:53")
	if err != nil {
		return nil, err
	}
	defer c.Close()

	addr := c.LocalAddr().(*net.UDPAddr)

	return addr.IP, nil
}
