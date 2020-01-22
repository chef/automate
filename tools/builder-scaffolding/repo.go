package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"net"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/lib/secrets"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var repoCmd = &cobra.Command{
	Use:  "repo",
	RunE: runRepo,
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

func runRepo(c *cobra.Command, args []string) error {
	ctx := context.Background()
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

	artifactRepo := backup.NewArtifactRepo(backupSpec)
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

	_, err = artifactRepo.Snapshot(ctx, snapshotName, bldrBucket, requiredArtifactsStream)
	if err != nil {
		logrus.WithError(err).Fatal("Failed to snapshot")
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
