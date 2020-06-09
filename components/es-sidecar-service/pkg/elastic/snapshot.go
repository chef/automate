package elastic

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"path"
	"regexp"
	"strconv"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"
)

/*
[backups]
backend = "fs"
[backups.fs]
  root_location = "/tmp/backup"
  max_snapshot_bytes_per_sec = "40m"
  max_restore_bytes_per_sec = "40m"
*/

const (
	// RepoBaseName is a basename that is used to namespace all snapshot repos
	RepoBaseName = "automate-elasticsearch-data"

	// deleteRetryInterval is the time to sleep between retrying snapshot deletes
	deleteInterval = 5 * time.Second
	// maxDeleteWaitTime is the maximum amount of time we'll wait for a delete to
	// be acknowledged
	maxDeleteWaitTime = 10 * time.Minute
)

// BackupsConfig contains settings for Es snapshot repo type and options, as
// expressed in the service's toml config file
type BackupsConfig struct {
	Backend          string           `mapstructure:"backend"`
	VerifyRepo       bool             `mapstructure:"verify_repo"`
	FsBackupsConfig  FsBackupsConfig  `mapstructure:"fs"`
	S3BackupsConfig  S3BackupsConfig  `mapstructure:"s3"`
	GCSBackupsConfig GCSBackupsConfig `mapstructure:"gcs"`
}

// BackupsDisabled returns true if the backend is set to disabled
func (c *BackupsConfig) BackupsDisabled() bool {
	return c.Backend == "disable"
}

// FsBackupsConfig represents the settings available for "fs" type Es snapshot
// repos
type FsBackupsConfig struct {
	RootLocation           string `mapstructure:"root_location"`
	MaxSnapshotBytesPerSec string `mapstructure:"max_snapshot_bytes_per_sec"`
	MaxRestoreBytesPerSec  string `mapstructure:"max_restore_bytes_per_sec"`
}

// CreateSnapshotStatus holds status information about a (possibly in-progress)
// Es snapshot. It basically contains the same info as the
// &api.CreateSnapshotStatusResponse type but is implemented separately to keep
// API concerns from leaking into this package
type CreateSnapshotStatus struct {
	State              string
	ProgressPercentage float64
	Message            string
}

// RestoreSnapshotStatus holds status information about a (possibly in-progress)
// Es snapshot restore. It basically contains the same info as the
// &api.RestoreSnapshotStatus type but is implemented separately to keep
// API concerns from leaking into this package
type RestoreSnapshotStatus struct {
	State              string
	ProgressPercentage float64
	Message            string
}

type createRepoReq struct {
	Type     string                 `json:"type"`
	Settings map[string]interface{} `json:"settings"`
}

type createSnapshotReq struct {
	Indices           string `json:"indices"`
	IgnoreUnavailable bool   `json:"ignore_unavailable"`
	AllowNoIndices    bool   `json:"allow_no_indices"`
}

type multiSnapshotStatus struct {
	Snapshots []snapshotStatus `json:"snapshots"`
}

type snapshotStatus struct {
	Name               string                  `json:"snapshot"`
	Repository         string                  `json:"repository"`
	UUID               string                  `json:"uuid"`
	State              string                  `json:"state"`
	IncludeGlobalState bool                    `json:"include_global_state"`
	ShardsStats        shardsStats             `json:"shards_stats"`
	Indices            map[string]indicesStats `json:"indices"`
}

type shardsStats struct {
	Initializing int `json:"initializing"`
	Started      int `json:"started"`
	Finalizing   int `json:"finalizing"`
	Done         int `json:"done"`
	Failed       int `json:"failed"`
	Total        int `json:"total"`
}

type indicesStats struct {
	ShardsStats shardsStats `json:"shards_stats"`
}

// ClusterHealth contains data returned by Es's cluster health API.
// See also: https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-health.html
type ClusterHealth struct {
	ClusterName         string `json:"cluster_name"`
	Status              string `json:"status"`
	NumberOfNodes       int    `json:"number_of_nodes"`
	NumberOfDataNodes   int    `json:"number_of_data_nodes"`
	ActivePrimaryShards int    `json:"active_primary_shards"`
	ActiveShards        int    `json:"active_shards"`
}

// Responses from the indices recovery API.
// See also: https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-recovery.html
type indicesRecoveryStats map[string]indexRecoveryStats

// Returns a copy of indicesRecoveryStats with only snapshot recovery stats;
// the recovery API by default includes information on other cluster activities
// such as relocating shards
func (s *indicesRecoveryStats) snapshots() *indicesRecoveryStats {
	snapshotStats := make(indicesRecoveryStats)
	for indexName, indexStats := range *s {
		for _, shardStats := range indexStats.Shards {
			if shardStats.Type == "SNAPSHOT" {
				var shards []shardsRecoveryStats
				// ensure snapshotStats[indexName] exists
				if _, exist := snapshotStats[indexName]; exist {
					shards = snapshotStats[indexName].Shards
				}
				// append shardStats to the list. We have to workaround go's limits on
				// what's allowed with maps.
				newShards := append(shards, shardStats)
				snapshotStats[indexName] = indexRecoveryStats{Shards: newShards}
			}
		}
	}
	return &snapshotStats
}

type indexRecoveryStats struct {
	Shards []shardsRecoveryStats `json:"shards"`
}

type shardsRecoveryStats struct {
	ID    int    `json:"id"`
	Type  string `json:"type"`
	Stage string `json:"stage"`
}

// CreateSnapshot creates/updates a repo with appropriate settings for the
// BackupsConfig, then creates a snapshot with the given snapshotName
// containing indices matching the multiIndexSpec.
// See also: https://www.elastic.co/guide/en/elasticsearch/reference/current/multi-index.html
func (es *Elastic) CreateSnapshot(ctx context.Context, serviceName, snapshotName, multiIndexSpec string, bc *BackupsConfig) error {
	repos, err := es.repoNamesFor(ctx, serviceName)
	if err != nil {
		return errors.Wrapf(err, "failed to generate snapshot repository name for %s", serviceName)
	}
	// Always use the latest repo for new snapshots
	repoName := repos[0]

	// create/update repo for our snapshot
	if err := es.CreateSnapshotRepository(ctx, repoName, bc); err != nil {
		return errors.Wrapf(err,
			"failed to create snapshot repository %s for snapshot %q of service %q",
			repoName, snapshotName, serviceName,
		)
	}

	req := createSnapshotReq{
		Indices:           multiIndexSpec,
		IgnoreUnavailable: true,
		AllowNoIndices:    true,
	}

	res, err := es.client.SnapshotCreate(repoName, snapshotName).BodyJson(req).Do(ctx)
	if err != nil {
		return errors.Wrapf(err, "failed to create es snapshot %q for service %q", snapshotName, serviceName)
	}

	log.WithFields(log.Fields{
		"service_name":              serviceName,
		"snapshot_name":             snapshotName,
		"snapshot_repository":       repoName,
		"snapshot_request_accepted": *res.Accepted,
	}).Info("Created snapshot")

	return nil
}

// GetCreateSnapshotStatus calls the Es snapshot status API and returns the
// result. It is used to track the progress of a snapshot asynchronously.
// See also https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-snapshots.html
func (es *Elastic) GetCreateSnapshotStatus(ctx context.Context, serviceName, snapshotName string) (CreateSnapshotStatus, error) {
	repoName, err := es.findRepoForSnapshot(ctx, serviceName, snapshotName)
	if err != nil {
		return CreateSnapshotStatus{}, err
	}

	path := fmt.Sprintf("/_snapshot/%s/%s/_status", repoName, snapshotName)
	response, err := es.client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "GET",
		Path:   path,
		Params: url.Values{},
	})

	if err != nil {
		return CreateSnapshotStatus{}, errors.Wrapf(err,
			"failed to find status of snapshot %q for service %q",
			snapshotName, serviceName,
		)
	}

	snapshotStatusList := multiSnapshotStatus{}

	err = json.Unmarshal(response.Body, &snapshotStatusList)
	if err != nil {
		return CreateSnapshotStatus{}, errors.Wrapf(err,
			"Invalid JSON response for status of snapshot %q for service %q",
			snapshotName, serviceName,
		)
	}

	switch len(snapshotStatusList.Snapshots) {
	case 0:
		// we should get a 404 error requesting snapshot status before we reach this case
		return CreateSnapshotStatus{}, errors.Errorf("No snapshot named %q found in repo %q", snapshotName, repoName)
	case 1:
	default:
		// should be impossible?
		return CreateSnapshotStatus{}, errors.Errorf("Multiple snapshots matching %q found in repo %q", snapshotName, repoName)
	}

	esSnapshotStatus := snapshotStatusList.Snapshots[0]

	s := CreateSnapshotStatus{}
	switch esSnapshotStatus.State {
	case "INIT", "STARTED", "IN_PROGRESS":
		s.State = "IN_PROGRESS"
	case "SUCCESS":
		s.State = "SUCCESS"
	default:
		s.State = "FAILED"
	}

	// Tracking the progress by shards completed/remaining is the only way we've
	// found to report status; the bytes stats included in the messages update
	// the totals as they go so they're not reliable. This approach has proved
	// reasonable when used for A1 upgrades in our testing. See also:
	// https://github.com/chef/automate/blob/13e626b60a9ae391a2b70e9d50970b8ddb6742c8/components/automate-deployment/pkg/a1upgrade/a1commands.go#L253
	s.ProgressPercentage = (float64(esSnapshotStatus.ShardsStats.Done) / float64(esSnapshotStatus.ShardsStats.Total)) * 100
	s.Message = ""

	// Backup will hammer this endpoint repeatedly waiting for success so bump message down to debug
	log.WithFields(log.Fields{
		"state":               s.State,
		"service_name":        serviceName,
		"snapshot_name":       snapshotName,
		"snapshot_repository": repoName,
		"progress":            s.ProgressPercentage,
	}).Debug("fetched snapshot status")

	return s, nil
}

// RestoreSnapshot restores the given snapshot of the given service.
func (es *Elastic) RestoreSnapshot(ctx context.Context, serviceName, snapshotName string, bc *BackupsConfig) error {
	// HACK: If you're restoring an elasticsearch cluster you have to recreate the
	// the snapshot repository in order to list it. This means that in some cases
	// we may create new empty legacy snapshot repos to check for legacy snapshots.
	// We should investigate if there's state that we can backup and restore to
	// prevent having to do this.
	repos, err := es.repoNamesFor(ctx, serviceName)
	if err != nil {
		return errors.Wrapf(err, "failed to generate snapshot repository names for %s", serviceName)
	}

	for _, repo := range repos {
		if err = es.ensureRepo(ctx, repo, bc); err != nil {
			return err
		}
	}

	repoName, err := es.findRepoForSnapshot(ctx, serviceName, snapshotName)
	if err != nil {
		return err
	}

	ctxLog := log.WithFields(log.Fields{
		"service_name":        serviceName,
		"snapshot_name":       snapshotName,
		"snapshot_repository": repoName,
	})

	path := fmt.Sprintf("/_snapshot/%s/%s/_restore", repoName, snapshotName)

	indexesList, _, err := es.indicesAndShardsInSnapshot(ctx, repoName, snapshotName)
	if err != nil {
		return errors.Wrap(err, "failed fetching snapshot data before restore")
	}

	for _, indexName := range indexesList {
		ctxLog.WithField("index", indexName).Debug("closing index")

		_, err := es.client.CloseIndex(indexName).IgnoreUnavailable(true).Do(ctx)
		if err != nil {
			return errors.Wrapf(err,
				"failed to close index %q before restore of snapshot %q in repo %q",
				indexName, snapshotName, repoName,
			)
		}
	}

	response, err := es.client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "POST",
		Path:   path,
		Params: url.Values{},
	})
	if err != nil {
		return errors.Wrapf(err,
			"failed to restore snapshot %q for service %q",
			snapshotName, serviceName,
		)
	}

	var responseData map[string]interface{}

	err = json.Unmarshal(response.Body, &responseData)
	if err != nil {
		ctxLog.WithField("raw_response", response.Body).
			WithError(err).
			Debug("invalid JSON in snapshot restore response")
		return errors.Wrapf(err,
			"invalid JSON in server response from request to restore snapshot %q for service %q",
			snapshotName, serviceName,
		)
	}

	err = es.waitForYellow(ctx)
	if err != nil {
		return errors.Wrapf(err,
			"failure waiting for Es cluster to stabilize after restoring %q from repo %q",
			snapshotName, repoName,
		)
	}

	// If the snapshot somehow contains no indices then we can early return as
	// we don't need to wait for them to recover
	if len(indexesList) == 0 {
		ctxLog.Warn("No indices found in snapshot")
		return nil
	}

	// The recovery API takes a short time to start returning information about
	// the restore (about 20ms in local testing); we wait for it to become
	// populated so subsequent calls to get restore status will be able to get
	// status info (otherwise the status endpoint would need to tolerate empty
	// responses from the recovery API and return an "UNKNOWN" status response,
	// which we'd need to accommodate throughout the stack)
	//
	// We use exponential backoff for a total wait time around 10s just in case a
	// customer's system is really sluggish, which we have seen on larger clusters
	// or when snapshots are being restored from a remote repositories in the cloud.
	for tries := uint(0); tries < 10; tries++ {
		recoveryInfo, err := es.indicesSnapshotRecoveryStatus(ctx)
		ctxLog = ctxLog.WithField("recovery_info", recoveryInfo)
		if err != nil {
			ctxLog.WithError(err).Debug("failed to get snapshot recovery status")
			return err
		}

		if len(*recoveryInfo) != 0 {
			ctxLog.WithField("snapshot_request_response", responseData).Debug("snapshot recovery started")
			return nil
		}

		sleepMillis := time.Duration(10*(1<<tries)) * time.Millisecond
		time.Sleep(sleepMillis)
	}

	// If we get here, recovery API never came up :(
	return errors.Errorf("Failed to make restore status data available after restoring %q from repo %q",
		snapshotName, repoName,
	)
}

// GetRestoreSnapshotStatus gives the status of a possibly in-progress snapshot restore operation.
func (es *Elastic) GetRestoreSnapshotStatus(ctx context.Context, serviceName, snapshotName string) (*RestoreSnapshotStatus, error) {
	repoName, err := es.findRepoForSnapshot(ctx, serviceName, snapshotName)
	if err != nil {
		return nil, err
	}

	ctxLog := log.WithFields(log.Fields{
		"action":              "GetRestoreSnapshotStatus",
		"service_name":        serviceName,
		"snapshot_name":       snapshotName,
		"snapshot_repository": repoName,
	})

	indexesList, _, err := es.indicesAndShardsInSnapshot(ctx, repoName, snapshotName)
	if err != nil {
		return nil, err
	}

	recoveryStatus, err := es.indicesSnapshotRecoveryStatus(ctx)

	if err != nil {
		ctxLog.WithError(err).Debug("failed to get recovery status for restore")
		return nil, err
	}

	s := RestoreSnapshotStatus{
		State:              "SUCCESS",
		ProgressPercentage: float64(100),
	}

	idxComplete := 0
	for _, indexName := range indexesList {
		indexStatus, exist := (*recoveryStatus)[indexName]
		if !exist {
			s.State = "IN_PROGRESS"
			break
		}
		for _, shardStats := range indexStatus.Shards {
			if shardStats.Stage != "DONE" {
				s.State = "IN_PROGRESS"
				break
			}
		}
		idxComplete++
	}

	if s.State == "SUCCESS" {
		ctxLog.Debug("restore completed")
		return &s, nil
	}

	s.ProgressPercentage = (float64(idxComplete) / float64(len(indexesList))) * 100
	ctxLog.WithFields(log.Fields{
		"snapshot_restore_state": s.State,
		"num_indexes":            len(indexesList),
		"completed_indexes":      idxComplete,
	}).Debug("got snapshot status")
	return &s, nil
}

type deleteResponse struct {
	Acknowledged bool `json:"acknowledged"`
}

// DeleteSnapshot takes a context, service name and snapshot name and deletes
// the snapshot.
func (es *Elastic) DeleteSnapshot(ctx context.Context, serviceName, snapshotName string) error {
	ctx, cancel := context.WithTimeout(ctx, maxDeleteWaitTime)
	defer cancel()
	ctxLog := log.WithFields(log.Fields{
		"action":        "DeleteSnapshot",
		"service_name":  serviceName,
		"snapshot_name": snapshotName,
	})

	repo, err := es.findRepoForSnapshot(ctx, serviceName, snapshotName)

	if err != nil {
		if IsSnapshotNotFound(err) {
			return nil
		}
		return err
	}
	ctxLog = ctxLog.WithField("snapshot_repository", repo)

	path := fmt.Sprintf("/_snapshot/%s/%s", repo, snapshotName)
	response, err := es.client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "DELETE",
		Path:   path,
		Params: url.Values{},
		Retrier: elastic.RetrierFunc(func(ctx context.Context, retry int,
			req *http.Request, resp *http.Response, err error) (time.Duration, bool, error) {
			// ES does not allow multiple snapshot deletes to happen in parallel.
			// It will return a 503 when you try. This will fix that:
			// See https://github.com/chef/automate/issues/2568
			if resp != nil && resp.StatusCode >= 500 {
				return deleteInterval, true, nil
			}
			return 0, false, nil
		}),
	})

	if err != nil {
		return errors.Wrapf(err, "failure deleting snapshot %q from repo %q", snapshotName, repo)
	}

	var ack deleteResponse
	err = json.Unmarshal(response.Body, &ack)
	if err != nil {
		ctxLog.WithFields(log.Fields{
			"raw_response": response.Body,
		}).WithError(err).Debug("Invalid JSON returned for snapshot delete")
		return errors.Wrapf(err,
			"Invalid JSON response from Es for snapshot delete; Raw data:\n%s\n",
			response.Body,
		)
	}

	if !ack.Acknowledged {
		return errors.Wrapf(err,
			"elasticsearch did not acknowledge request to delete snapshot %q from repo %q",
			snapshotName, repo,
		)
	}

	ctxLog.Info("deleted snapshot acknowledged")
	return nil
}

// CreateSnapshotRepository creates a snapshot repository for the given
// service, with type and settings as set in the backups config
// If the repo exists, it will be updated with any settings that have changed
func (es *Elastic) CreateSnapshotRepository(ctx context.Context, repoName string, bc *BackupsConfig) error {
	var req createRepoReq

	switch bc.Backend {
	case "s3":
		req = bc.S3BackupsConfig.createRepoReq(repoName)
	case "gcs":
		req = bc.GCSBackupsConfig.createRepoReq(repoName)
	default:
		req = bc.FsBackupsConfig.createRepoReq(repoName)
	}

	creator := es.client.SnapshotCreateRepository(repoName)
	creator.BodyJson(req)
	creator.Verify(bc.VerifyRepo)
	res, err := creator.Do(ctx)
	if err != nil {
		return errors.Wrapf(err, "elasticsearch repository create request failed for repo %q; request data: '%v'", repoName, req)
	}
	if !res.Acknowledged {
		return errors.Errorf("elasticsearch did not acknowledge repository create request for repo %q; request data '%v'", repoName, req)
	}

	return nil
}

// RemoveSnapshotRepository deletes a snapshot repository of the type set in
// the backups config
func (es *Elastic) RemoveSnapshotRepository(ctx context.Context, repoName string, bc *BackupsConfig) error {
	_, err := es.client.SnapshotDeleteRepository(repoName).Do(ctx)
	return err
}

func (es *Elastic) waitForYellow(ctx context.Context) error {
	waitForYellowTimeout, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()

	_, err := es.client.ClusterHealth().WaitForYellowStatus().Do(waitForYellowTimeout)
	return err
}

// IndicesAndShardsInSnapshot gives a list of the indices and count of the
// shards referenced by a snapshot.
func (es *Elastic) indicesAndShardsInSnapshot(ctx context.Context, repoName, snapshotName string) ([]string, int, error) {
	path := fmt.Sprintf("/_snapshot/%s/%s/_status", repoName, snapshotName)
	response, err := es.client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "GET",
		Path:   path,
		Params: url.Values{},
	})
	if err != nil {
		return []string{}, 0, errors.Wrapf(err, "failure requesting snapshot status while restoring %q from repo %q", snapshotName, repoName)
	}

	snapshotStatusList := multiSnapshotStatus{}

	err = json.Unmarshal(response.Body, &snapshotStatusList)
	if err != nil {
		return []string{}, 0, errors.Wrapf(err, "Invalid JSON response from Es for snapshot status %q; Raw data:\n%s\n", path, response.Body)
	}

	switch len(snapshotStatusList.Snapshots) {
	case 0:
		// we should get a 404 error requesting snapshot status before we reach this case
		return []string{}, 0, errors.Errorf("No snapshot named %q found in repo %q", snapshotName, repoName)
	case 1:
	default:
		// should be impossible?
		return []string{}, 0, errors.Errorf("Multiple snapshots matching %q found in repo %q", snapshotName, repoName)
	}

	// Number of shards we want available
	shardsToWaitFor := snapshotStatusList.Snapshots[0].ShardsStats.Total

	// Names of the indexes
	indexStats := snapshotStatusList.Snapshots[0].Indices
	indexesList := make([]string, 0, len(indexStats))
	for indexName := range indexStats {
		indexesList = append(indexesList, indexName)
	}
	return indexesList, shardsToWaitFor, nil
}

func (es *Elastic) indicesSnapshotRecoveryStatus(ctx context.Context) (*indicesRecoveryStats, error) {
	indicesRecoveryPath := "/_recovery"

	response, err := es.client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "GET",
		Path:   indicesRecoveryPath,
		Params: url.Values{"pretty": []string{"true"}},
	})

	if err != nil {
		return nil, errors.Wrapf(err, "Failed fetching indices recovery status")
	}

	recoveryData := make(indicesRecoveryStats)
	err = json.Unmarshal(response.Body, &recoveryData)
	if err != nil {
		return nil, errors.Wrapf(err, "Invalid JSON response from Es for indices recover status %q; Raw data:\n%s\n", indicesRecoveryPath, response.Body)
	}
	return recoveryData.snapshots(), nil
}

// ensureRepo takes a repository name and backup config and ensures that the
// repository exists. If the repository does not exist it will be created.
func (es *Elastic) ensureRepo(ctx context.Context, repoName string, bc *BackupsConfig) error {
	_, err := es.client.SnapshotGetRepository(repoName).Do(ctx)
	if elastic.IsNotFound(err) {
		if err = es.CreateSnapshotRepository(ctx, repoName, bc); err != nil {
			return errors.Wrap(err, "failed to create snapshot repository")
		}
	} else {
		return errors.Wrap(err, "failed to determine snapshot repository settings")
	}

	return nil
}

type errSnapshotNotFound struct {
	serviceName  string
	snapshotName string
}

// SnapshotNotFound error constructor
func SnapshotNotFound(serviceName string, snapshotName string) error {
	return errSnapshotNotFound{
		serviceName:  serviceName,
		snapshotName: snapshotName,
	}
}

func (e errSnapshotNotFound) Error() string {
	return fmt.Sprintf("failed to locate snapshot repo for service %s snapshot %s", e.serviceName, e.snapshotName)
}

// IsSnapshotNotFound returns true if the underlying error represents a
// snapshot not found error
func IsSnapshotNotFound(e error) bool {
	_, ok := e.(errSnapshotNotFound)
	return ok
}

// findRepoForSnapshot takes a context, service name and snapshot name searches current
// and legacy snapshot repositories for snapshot that matches the snapshot name.
// If a repository containing the snapshot cannot be found it will return an error.
func (es *Elastic) findRepoForSnapshot(ctx context.Context, serviceName, snapshotName string) (string, error) {
	var repoName string

	repos, err := es.repoNamesFor(ctx, serviceName)
	if err != nil {
		return repoName, errors.Wrapf(err, "failed to generate snapshot repository name for %s", serviceName)
	}

	// Check if a matching snapshot exists in the current snapshot repo
	ok, err := es.snapshotExist(ctx, repos[0], snapshotName)
	if err != nil {
		return "", err
	}
	if ok {
		return repos[0], nil
	}

	// Try the legacy repo
	ok, err = es.snapshotExist(ctx, repos[1], snapshotName)
	if err != nil {
		return "", err
	}
	if ok {
		return repos[1], nil
	}

	return repoName, SnapshotNotFound(serviceName, snapshotName)
}

// snapshotExists takes a context, repository named and snapshot name and returns
// true if the snapshot exists in the given repository.
func (es *Elastic) snapshotExist(ctx context.Context, repoName, snapshotName string) (bool, error) {
	path := fmt.Sprintf("/_snapshot/%s/%s", repoName, snapshotName)
	log.WithField("path", path).Debug("checking if snapshot exist")
	res, err := es.client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "GET",
		Path:   path,
		Params: url.Values{},
	})

	if elastic.IsNotFound(err) {
		return false, nil
	}

	if err != nil {
		log.WithField("response", res).WithError(err).Info("failed to determine snapshot status")
		return false, err
	}

	return true, nil
}

// repoNamesFor returns the possible repository names that may be used for a
// services snaphots. It defaults to a namespace that includes the current major
// version of elasticsearch to make it easy to determine snapshot compatibility.
// Down the road it's possible that we'll need to support n-1 major version repos.
// As such, we return a second legacy repository name with an n-1 namespace that
// can be used to delete or restore old snapshots.
// This follows elastic's recommendations:
// https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-snapshots.html#_repositories
func (es *Elastic) repoNamesFor(ctx context.Context, serviceName string) ([]string, error) {
	stats, err := es.client.ClusterStats().Do(ctx)
	if err != nil {
		return nil, err
	}

	major, err := clusterVersion(stats.Nodes.Versions)
	if err != nil {
		return nil, err
	}

	return []string{
		fmt.Sprintf("chef-automate-es%d-%s", major, serviceName),
		fmt.Sprintf("chef-automate-es%d-%s", major-1, serviceName),
	}, nil
}

// createRepoReq returns a createRepoReq from the FsBackupsConfig
func (fs *FsBackupsConfig) createRepoReq(repoName string) createRepoReq {
	req := createRepoReq{
		Type: "fs",
		Settings: map[string]interface{}{
			"location": path.Join(fs.RootLocation, RepoBaseName, repoName),
		},
	}

	if msbps := fs.MaxSnapshotBytesPerSec; msbps != "" {
		req.Settings["max_snapshot_bytes_per_sec"] = msbps
	}

	if mrbps := fs.MaxRestoreBytesPerSec; mrbps != "" {
		req.Settings["max_restore_bytes_per_sec"] = mrbps
	}

	return req
}

// clusterVersion takes a slice of semantic version strings and returns the
// highest major version. If any of the versions are not valid semantic versions,
// or there are multiple different major version it will return an error.
func clusterVersion(versions []string) (int, error) {
	verRegex := regexp.MustCompile(`(\d+)\.\d+\.\d+`)
	var version int

	for i, ver := range versions {
		matches := verRegex.FindStringSubmatch(ver)
		if len(matches) < 2 {
			return version, errors.Errorf("%s is not a valid version", ver)
		}

		major, err := strconv.Atoi(matches[1])
		if err != nil {
			return version, errors.Errorf("%s could not convert version to int", ver)
		}

		if i == 0 {
			version = major
		}

		if version != major {
			return version, errors.Errorf("Multiple major Elasticsearch versions found in cluster: '%d' and '%d'.", version, major)
		}
	}

	return version, nil
}
