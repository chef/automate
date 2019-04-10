package integration_test

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"os"
	"testing"
	"time"

	es "github.com/olivere/elastic"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
)

const snapshotRoot = "/var/opt/chef-automate/backups/test"

func completeFsBackupConfig() *elastic.BackupsConfig {
	return &elastic.BackupsConfig{
		Backend: "fs",
		FsBackupsConfig: elastic.FsBackupsConfig{
			RootLocation:           snapshotRoot,
			MaxSnapshotBytesPerSec: "100M",
			MaxRestoreBytesPerSec:  "200M",
		},
	}

}

func cleanupRepos(t *testing.T) {
	ctx := context.Background()
	_, err := suite.esClient.SnapshotDeleteRepository("*").Do(ctx)
	require.NoError(t, err)
	repoMetadataMap, err := suite.esClient.SnapshotGetRepository("*").Do(ctx)
	require.NoError(t, err)
	require.Empty(t, repoMetadataMap)
	// Es doesn't nuke the data when you delete the repo:
	os.RemoveAll(snapshotRoot)
}

func TestRepoCreate(t *testing.T) {
	ctx := context.Background()

	bc := completeFsBackupConfig()
	err := suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", bc)
	require.NoError(t, err)

	repoMetadataMap, err := suite.esClient.SnapshotGetRepository("*").Do(ctx)

	require.NoError(t, err)

	require.Contains(t, repoMetadataMap, "example-service")

	expectedRepo := *repoMetadataMap["example-service"]

	assert.Equal(t, "fs", expectedRepo.Type)
	assert.Equal(t, "100M", expectedRepo.Settings["max_snapshot_bytes_per_sec"])
	assert.Equal(t, "200M", expectedRepo.Settings["max_restore_bytes_per_sec"])

	cleanupRepos(t)
}

func TestRepoUpdate(t *testing.T) {
	ctx := context.Background()

	bc := completeFsBackupConfig()

	err := suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", bc)
	require.NoError(t, err)

	newBc := elastic.BackupsConfig{
		Backend: "fs",
		FsBackupsConfig: elastic.FsBackupsConfig{
			RootLocation:           snapshotRoot,
			MaxSnapshotBytesPerSec: "111M",
			MaxRestoreBytesPerSec:  "222M",
		},
	}

	err = suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", &newBc)

	require.NoError(t, err)

	repoMetadataMap, err := suite.esClient.SnapshotGetRepository("*").Do(ctx)

	require.NoError(t, err)
	require.Contains(t, repoMetadataMap, "example-service")

	expectedRepo := *repoMetadataMap["example-service"]

	assert.Equal(t, "fs", expectedRepo.Type)
	assert.Equal(t, "111M", expectedRepo.Settings["max_snapshot_bytes_per_sec"])
	assert.Equal(t, "222M", expectedRepo.Settings["max_restore_bytes_per_sec"])

	cleanupRepos(t)
}

func TestRepoUpdateResetDefaults(t *testing.T) {
	ctx := context.Background()

	bc := completeFsBackupConfig()
	err := suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", bc)
	require.NoError(t, err)

	newBc := elastic.BackupsConfig{
		Backend: "fs",
		FsBackupsConfig: elastic.FsBackupsConfig{
			RootLocation: snapshotRoot,
		},
	}

	err = suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", &newBc)

	require.NoError(t, err)

	repoMetadataMap, err := suite.esClient.SnapshotGetRepository("*").Do(ctx)

	require.NoError(t, err)
	require.Contains(t, repoMetadataMap, "example-service")

	expectedRepo := *repoMetadataMap["example-service"]

	assert.Equal(t, "fs", expectedRepo.Type)
	assert.NotContains(t, expectedRepo.Settings, "max_snapshot_bytes_per_sec")
	assert.NotContains(t, expectedRepo.Settings, "max_restore_bytes_per_sec")

	cleanupRepos(t)

}

func TestRemoveRepo(t *testing.T) {
	ctx := context.Background()

	bc := completeFsBackupConfig()
	err := suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", bc)
	require.NoError(t, err)

	err = suite.esSidecar.CreateSnapshotRepository(ctx, "another-example-service", bc)
	require.NoError(t, err)

	err = suite.esSidecar.RemoveSnapshotRepository(ctx, "example-service", bc)
	require.NoError(t, err)

	repoMetadataMap, err := suite.esClient.SnapshotGetRepository("*").Do(ctx)
	require.Contains(t, repoMetadataMap, "another-example-service")
	require.NotContains(t, repoMetadataMap, "example-service")

	cleanupRepos(t)
}

func listSnapshots(t *testing.T, repo string) map[string][]map[string]interface{} {
	ctx := context.Background()
	path := fmt.Sprintf("/_snapshot/%s/_all", repo)

	response, err := suite.esClient.PerformRequest(ctx, es.PerformRequestOptions{
		Method: "GET",
		Path:   path,
		Params: url.Values{},
	})

	require.NoError(t, err)

	snapshotList := make(map[string][]map[string]interface{})

	err = json.Unmarshal(response.Body, &snapshotList)
	require.NoError(t, err)

	return snapshotList
}

func waitForSnapshotSuccess(t *testing.T, repoName string) {
	for {
		allSnapshotStatus := listSnapshots(t, repoName)

		if allSnapshotStatus["snapshots"][0]["state"] != "IN_PROGRESS" {
			assert.Equal(t, "SUCCESS", allSnapshotStatus["snapshots"][0]["state"])
			break
		}
		time.Sleep(10 * time.Millisecond)

	}
}

func waitForRestoreSuccess(t *testing.T, serviceName, snapshotName string) {
	ctx := context.Background()
	for {
		s, err := suite.esSidecar.GetRestoreSnapshotStatus(ctx, serviceName, snapshotName)
		require.NoError(t, err)
		if s.State != "IN_PROGRESS" {
			break
		}
		time.Sleep(10 * time.Millisecond)
	}
}

const (
	includedIndex = "included-index"
	excludedIndex = "excluded-index"
)

func TestCreateSnapshot(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	ctx := context.Background()

	bc := completeFsBackupConfig()

	err := suite.esSidecar.MakeMeAnIndex(ctx, includedIndex)
	require.NoError(t, err)
	err = suite.esSidecar.MakeMeAnIndex(ctx, excludedIndex)
	require.NoError(t, err)

	err = suite.esSidecar.CreateSnapshot(ctx, "example-service", "snap-1", includedIndex, bc)
	require.NoError(t, err)

	// Repo should be implicitly created
	repoMetadataMap, err := suite.esClient.SnapshotGetRepository("*").Do(ctx)

	require.NoError(t, err)

	require.Contains(t, repoMetadataMap, "chef-automate-es6-example-service")

	expectedRepo := *repoMetadataMap["chef-automate-es6-example-service"]

	assert.Equal(t, "fs", expectedRepo.Type)
	assert.Equal(t, "100M", expectedRepo.Settings["max_snapshot_bytes_per_sec"])
	assert.Equal(t, "200M", expectedRepo.Settings["max_restore_bytes_per_sec"])

	snapshotList := listSnapshots(t, "chef-automate-es6-example-service")

	require.NotEmpty(t, snapshotList["snapshots"])

	assert.Equal(t, "snap-1", snapshotList["snapshots"][0]["snapshot"])
	assert.Contains(t, snapshotList["snapshots"][0]["indices"], includedIndex)
	assert.NotContains(t, snapshotList["snapshots"][0]["indices"], excludedIndex)

	waitForSnapshotSuccess(t, "chef-automate-es6-example-service")
}

func TestCreateSnapshotStatus(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	ctx := context.Background()

	bc := completeFsBackupConfig()

	err := suite.esSidecar.MakeMeAnIndex(ctx, includedIndex)
	require.NoError(t, err)
	err = suite.esSidecar.MakeMeAnIndex(ctx, excludedIndex)
	require.NoError(t, err)

	err = suite.esSidecar.CreateSnapshot(ctx, "example-service", "snap-1", includedIndex, bc)
	require.NoError(t, err)

	// If we fail, wait for snapshot to finish so the cleanup can succeed
	defer waitForSnapshotSuccess(t, "chef-automate-es6-example-service")

	status, err := suite.esSidecar.GetCreateSnapshotStatus(ctx, "example-service", "snap-1")
	require.NoError(t, err)

	// These tests pretty consistently get the status of the snapshot before it's
	// done, but that can't be guaranteed.
	require.Contains(t, []string{"IN_PROGRESS", "SUCCESS"}, status.State)
	require.Equal(t, "", status.Message)
	require.True(t, status.ProgressPercentage >= 0.0)
	require.True(t, status.ProgressPercentage <= 100.0)

	waitForSnapshotSuccess(t, "chef-automate-es6-example-service")

	status2, err := suite.esSidecar.GetCreateSnapshotStatus(ctx, "example-service", "snap-1")
	require.NoError(t, err)

	require.Equal(t, "SUCCESS", status2.State)
	require.Equal(t, "", status2.Message)
	require.Equal(t, 100.0, status2.ProgressPercentage)
}

var testDocs = []testDocument{
	testDocument{id_for_search: "3"},
	testDocument{id_for_search: "2"},
	testDocument{id_for_search: "1"},
}

func createDataForRestoreTests(t *testing.T, bc *elastic.BackupsConfig) {
	ctx := context.Background()

	err := suite.esSidecar.MakeMeAnIndex(ctx, includedIndex)
	require.NoError(t, err)
	err = suite.esSidecar.MakeMeAnIndex(ctx, excludedIndex)
	require.NoError(t, err)

	// put some data in here
	for _, doc := range testDocs {
		addDocToIndex(t, includedIndex, doc, doc.id_for_search)
		addDocToIndex(t, excludedIndex, doc, doc.id_for_search)
	}

	for _, doc := range testDocs {
		res, err := suite.esClient.Get().Index(includedIndex).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		assert.True(t, res.Found)

		res, err = suite.esClient.Get().Index(excludedIndex).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		assert.True(t, res.Found)
	}

}

func TestRestoreWhenRepoAndSnapshotMissing(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()

	createDataForRestoreTests(t, bc)

	ctx := context.Background()
	// Restore
	err := suite.esSidecar.RestoreSnapshot(ctx, "example-service", "missing-snapshot-name", bc)
	require.Error(t, err)
}

func TestRestoreWhenSnapshotMissing(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()

	createDataForRestoreTests(t, bc)

	ctx := context.Background()

	err := suite.esSidecar.CreateSnapshotRepository(ctx, "example-service", bc)
	require.NoError(t, err)

	// Restore
	err = suite.esSidecar.RestoreSnapshot(ctx, "example-service", "missing-snapshot-name", bc)
	require.Error(t, err)
}

func TestRestoreWhenDataDeleted(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()

	createDataForRestoreTests(t, bc)

	ctx := context.Background()

	// snapshot
	err := suite.esSidecar.CreateSnapshot(ctx, "example-service", "snap-1", includedIndex, bc)
	require.NoError(t, err)

	waitForSnapshotSuccess(t, "chef-automate-es6-example-service")

	// Delete everything
	for _, doc := range testDocs {
		_, err = suite.esClient.Delete().Index(includedIndex).Type(testTypeName).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		_, err = suite.esClient.Delete().Index(excludedIndex).Type(testTypeName).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
	}

	suite.DeleteAllIndices()

	// Check that everything is deleted
	for _, doc := range testDocs {
		_, err := suite.esClient.Get().Index(includedIndex).Id(doc.id_for_search).Do(ctx)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404")

		_, err = suite.esClient.Get().Index(excludedIndex).Id(doc.id_for_search).Do(ctx)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404")
	}

	// Restore
	err = suite.esSidecar.RestoreSnapshot(ctx, "example-service", "snap-1", bc)
	require.NoError(t, err)

	waitForRestoreSuccess(t, "example-service", "snap-1")

	// includedIndex docs should be restored
	for _, doc := range testDocs {
		res, err := suite.esClient.Get().Index(includedIndex).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		assert.True(t, res.Found)
	}

	// excludedIndex docs should not be restored
	for _, doc := range testDocs {
		_, err := suite.esClient.Get().Index(excludedIndex).Id(doc.id_for_search).Do(ctx)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404")
	}

}

func TestRestoreWhenIndicesPresent(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()

	createDataForRestoreTests(t, bc)

	ctx := context.Background()

	// snapshot
	err := suite.esSidecar.CreateSnapshot(ctx, "example-service", "snap-1", includedIndex, bc)
	require.NoError(t, err)

	waitForSnapshotSuccess(t, "chef-automate-es6-example-service")

	// Delete everything
	for _, doc := range testDocs {
		_, err = suite.esClient.Delete().Index(includedIndex).Type(testTypeName).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		_, err = suite.esClient.Delete().Index(excludedIndex).Type(testTypeName).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
	}

	// NOTE: indices are NOT deleted, that's the case we're specifically testing here.

	// Check that everything is deleted
	for _, doc := range testDocs {
		_, err := suite.esClient.Get().Index(includedIndex).Id(doc.id_for_search).Do(ctx)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404")

		_, err = suite.esClient.Get().Index(excludedIndex).Id(doc.id_for_search).Do(ctx)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404")
	}

	// Restore
	err = suite.esSidecar.RestoreSnapshot(ctx, "example-service", "snap-1", bc)
	require.NoError(t, err)

	waitForRestoreSuccess(t, "example-service", "snap-1")

	// includedIndex docs should be restored
	for _, doc := range testDocs {
		res, err := suite.esClient.Get().Index(includedIndex).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		assert.True(t, res.Found)
	}

	// excludedIndex docs should not be restored
	for _, doc := range testDocs {
		_, err := suite.esClient.Get().Index(excludedIndex).Id(doc.id_for_search).Do(ctx)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404")
	}
}

func TestRestoreStatus(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()

	createDataForRestoreTests(t, bc)

	ctx := context.Background()

	// snapshot
	err := suite.esSidecar.CreateSnapshot(ctx, "example-service", "snap-1", includedIndex, bc)
	require.NoError(t, err)

	waitForSnapshotSuccess(t, "chef-automate-es6-example-service")

	// Delete everything
	for _, doc := range testDocs {
		_, err = suite.esClient.Delete().Index(includedIndex).Type(testTypeName).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
		_, err = suite.esClient.Delete().Index(excludedIndex).Type(testTypeName).Id(doc.id_for_search).Do(ctx)
		require.NoError(t, err)
	}

	suite.DeleteAllIndices()

	// Restore
	err = suite.esSidecar.RestoreSnapshot(ctx, "example-service", "snap-1", bc)
	require.NoError(t, err)

	for {
		s, err := suite.esSidecar.GetRestoreSnapshotStatus(ctx, "example-service", "snap-1")
		require.NoError(t, err)
		if s.State != "IN_PROGRESS" {
			break
		}
		time.Sleep(10 * time.Millisecond)
	}
	s, err := suite.esSidecar.GetRestoreSnapshotStatus(ctx, "example-service", "snap-1")
	require.NoError(t, err)
	assert.Equal(t, "SUCCESS", s.State)
	assert.Equal(t, float64(100), s.ProgressPercentage)
}

func TestRestoreStatusForMissingSnapshot(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()
	createDataForRestoreTests(t, bc)

	ctx := context.Background()

	err := suite.esSidecar.CreateSnapshotRepository(ctx, "chef-automate-es6-example-service", bc)
	require.NoError(t, err)

	_, err = suite.esSidecar.GetRestoreSnapshotStatus(ctx, "example-service", "snap-1")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "failed to locate snapshot repo")
}

func TestDeleteSnapshot(t *testing.T) {
	// cleanup
	defer suite.DeleteAllIndices()
	defer cleanupRepos(t)

	bc := completeFsBackupConfig()
	createDataForRestoreTests(t, bc)

	ctx := context.Background()

	// snapshot
	err := suite.esSidecar.CreateSnapshot(ctx, "example-service", "snap-1", includedIndex, bc)
	require.NoError(t, err)

	waitForSnapshotSuccess(t, "chef-automate-es6-example-service")

	err = suite.esSidecar.DeleteSnapshot(ctx, "example-service", "snap-1")
	require.NoError(t, err)

	list := listSnapshots(t, "chef-automate-es6-example-service")
	assert.Empty(t, list["snapshots"])

	// Make sure it's idempotent
	err = suite.esSidecar.DeleteSnapshot(ctx, "example-service", "snap-1")
	require.NoError(t, err)
}
