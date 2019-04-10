// Copyright © 2017 Chef Software

package a1upgrade

import (
	"bufio"
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/pkg/errors"
	"gopkg.in/cheggaaa/pb.v1"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
)

// RabbitStats maps the JSON from rabbit's management API to a struct. We
// discard almost all of it except for the Messages count.
type RabbitStats struct {
	Messages int `json:"messages"`
}

// EsAggSnapshotStats represents the JSON messages returned by elasticsearch's
// snapshot status API. The API is reached at a URL like:
//
// http(s)://host:port/_snapshot/BACKUP_REPO_NAME/SNAPSHOT_NAME/_status
//
// Since elasticsearch supports requesting the status of multiple snapshots in
// one request, the top-level json object is a one-key map of "snapshots" to an
// Array of status objects for individual snapshots:
// {
//   "snapshots": [ snap1_stats, snap2_stats, ... ]
// }
type EsAggSnapshotStats struct {
	// Snapshots is the JSON array of status objects for each requested snapshot
	Snapshots []EsSnapshotStats `json:"snapshots"`
}

// EsSnapshotStats represent the status of individual snapshots, as returned by
// elasticsearch's snapshot status API.
type EsSnapshotStats struct {
	// State can be "IN_PROGRESS", "STARTED", "SUCCESS", or one of a few failure
	// states. es5 and below use "STARTED", es6 uses "IN_PROGRESS"
	State string `json:"state"`
	// ShardsStats contains stats about the progress of the snapshot in terms of
	// shards. It's the only reliable way to track the progress of the snapshot,
	// as elasticsearch does not pre-compute the number of bytes needed for a
	// snapshot and therefore the "total_size_in_bytes" metric in the "stats"
	// object is not constant.
	ShardsStats EsShardsStats `json:"shards_stats"`
}

// EsShardsStats is the JSON object in an elasticsearch snapshot status message
// that contains status about how many shards there are and how many have been
// snapshotted
type EsShardsStats struct {
	Done  int `json:"done"`
	Total int `json:"total"`
}

// MaintDotFlag is the path to the file which will make A1's nginx go into
// maintenance mode. See also:
// https://github.com/chef/automate/blob/e1278c5fbb4478fa31b0853788cad1f6714fecf7/cookbooks/delivery/templates/default/nginx-internal.conf.erb#L146
const MaintDotFlag = "/var/opt/delivery/delivery/etc/maint.flag"

const rabbitStatsURL = "https://localhost:15672/api/queues/%2Finsights/data-collector"

var duTimeout = 300 * time.Second
var automateCtlTimeout = 300 * time.Second
var systemctlTimeout = 300 * time.Second
var defaultCommandExecutor = command.NewExecExecutor()

// SystemCtlStopDelivery runs `systemctl stop delivery-runsvdir-start.service`
// Succeeds if delivery is stopped or was already stopped
func SystemCtlStopDelivery() error {
	return defaultCommandExecutor.Run("systemctl",
		command.Args("stop", "delivery-runsvdir-start.service"),
		command.Timeout(systemctlTimeout))
}

// SystemCtlDisableDelivery runs `systemctl disable delivery-runsvdir-start.service`
// Succeeds if delivery is disabled or was already disabled
func SystemCtlDisableDelivery() error {
	return defaultCommandExecutor.Run("systemctl",
		command.Args("disable", "delivery-runsvdir-start.service"),
		command.Timeout(systemctlTimeout))
}

// SystemCtlIsEnabledDelivery runs `systemctl is-enabled delivery-runsvdir-start.service`
func SystemCtlIsEnabledDelivery() bool {
	err := defaultCommandExecutor.Run("systemctl",
		command.Args("is-enabled", "delivery-runsvdir-start.service"),
		command.Timeout(systemctlTimeout))
	return err == nil
}

// AutomateCtlStop runs `automate-ctl stop` -- used to shut down A1 during upgrades.
func AutomateCtlStop() error {
	return defaultCommandExecutor.Run("automate-ctl",
		command.Args("stop"),
		command.Timeout(automateCtlTimeout))
}

// SystemCtlIsEnabledChefServer runs `systemctl is-enabled private_chef-runsvdir-start.service`
func SystemCtlIsEnabledChefServer() bool {
	err := defaultCommandExecutor.Run("systemctl",
		command.Args("is-enabled", "private_chef-runsvdir-start.service"),
		command.Timeout(systemctlTimeout))
	return err == nil
}

// ChefServerCtlStop stops all Chef Server services via
// chef-server-ctl.
func ChefServerCtlStop() error {
	return defaultCommandExecutor.Run("chef-server-ctl",
		command.Args("stop"),
		command.Timeout(automateCtlTimeout))
}

// ChefServerCtlStopService stops the given service using
// chef-server-ctl.
func ChefServerCtlStopService(svcName string) error {
	return defaultCommandExecutor.Run("chef-server-ctl",
		command.Args("stop", svcName),
		command.Timeout(automateCtlTimeout))
}

// SystemCtlStopChefServer runs `systemctl stop private_chef-runsvdir-start.service`
// Succeeds if Chef Server is stopped or was already stopped
func SystemCtlStopChefServer() error {
	return defaultCommandExecutor.Run("systemctl",
		command.Args("stop", "private_chef-runsvdir-start.service"),
		command.Timeout(systemctlTimeout))
}

// SystemCtlDisableChefServer runs `systemctl disable private_chef-runsvdir-start.service`
// Succeeds if Chef Server is disabled or was already disabled
func SystemCtlDisableChefServer() error {
	return defaultCommandExecutor.Run("systemctl",
		command.Args("disable", "private_chef-runsvdir-start.service"),
		command.Timeout(systemctlTimeout))
}

// AutomateCtlCreateBackup runs `automate-ctl create-backup` -- used to back up A1 during upgrades.
func AutomateCtlCreateBackup(name string) error {
	return defaultCommandExecutor.Run("automate-ctl",
		command.Args("create-backup", "--no-wait", name),
		command.Timeout(automateCtlTimeout))
}

// AutomateCtlStatus runs `automate-ctl status` -- used to check automate v1 health before upgrading
func AutomateCtlStatus() error {
	return defaultCommandExecutor.Run("automate-ctl",
		command.Args("status"),
		command.Timeout(automateCtlTimeout))
}

func RetrieveWebuiPrivKey() (string, error) {
	return getChefServerSecret("chef-server", "webui_key")
}

func RetrieveWebuiPubKey() (string, error) {
	return getChefServerSecret("chef-server", "webui_pub_key")
}

func RetrievePivotalKey() (string, error) {
	return getChefServerSecret("chef-server", "superuser_key")
}

func getChefServerSecret(secretGroup string, secretName string) (string, error) {
	output, err := defaultCommandExecutor.Output("chef-server-ctl",
		command.Args("show-secret", secretGroup, secretName),
		command.Timeout(automateCtlTimeout))
	if err != nil {
		return output, errors.Errorf("chef-server-ctl show secret failed: %s\nstdout:\n%s\nstderr:\n%s\n",
			err.Error(),
			output,
			command.StderrFromError(err))
	}
	return output, nil
}

// PubKeyFromPriv extracts the public key from a PEM-encoded PKCS1
// private key and returns it as a PEM-encoded PKIX public key.
//
// Currently the erchef bootstrap code expects the public key to be on
// on disk if the private key is on disk, so we use this to create the
// public key from the private key during the Chef Server credential
// migration.
func PubKeyFromPriv(keyData string) (string, error) {
	block, _ := pem.Decode([]byte(keyData))
	if block == nil {
		return "", errors.New("could not pem decode private key data")
	}

	key, err := x509.ParsePKCS1PrivateKey(block.Bytes)
	if err != nil {
		return "", errors.Wrap(err, "could not parse private key data")
	}

	pubKeyBytes, err := x509.MarshalPKIXPublicKey(&key.PublicKey)
	if err != nil {
		return "", errors.Wrap(err, "could not marshal public key data")
	}

	ret := new(strings.Builder)
	err = pem.Encode(ret, &pem.Block{Type: "PUBLIC KEY", Bytes: pubKeyBytes})
	if err != nil {
		return "", errors.Wrap(err, "could not encode public key as PEM")
	}

	return ret.String(), nil
}

// WaitForEsSnapshot polls elasticsearch snapshot status at the given esURL and
// displays a progressbar showing the shard-wise progress of the snapshot
func WaitForEsSnapshot(w cli.FormatWriter, esURL, repoType, name string) error {
	client := insecureHTTPSClient()
	_, totalShards, err := esBackupShardsProgress(client, esURL, repoType, name)
	if err != nil {
		return err
	}
	w.Bodyf("Waiting for elasticsearch to snapshot %d shards", totalShards)

	bar := pb.StartNew(totalShards)
	sleep := 1 * time.Second
	for {
		completedShards, _, err := esBackupShardsProgress(client, esURL, repoType, name)
		if err != nil {
			bar.Finish()
			return err
		}
		bar.Set(completedShards)
		if completedShards == totalShards {
			bar.Finish()
			return nil
		}
		time.Sleep(sleep)
	}
}

func esBackupShardsProgress(client *http.Client, esURL, repoType, name string) (completed int, total int, err error) {
	// In a1 the es snapshot repository names are hardcoded like so:
	// (automate-ctl/lib/backup/create.rb) repository = "#{config["delivery"]["backup"]["type"]}-chef-automate"
	repoName := fmt.Sprintf("%s-chef-automate", repoType)
	// e.g.:
	// http://localhost:8080/elasticsearch/_snapshot/s3-chef-automate/dan-test-2/_status?pretty=true
	url := fmt.Sprintf("%s/_snapshot/%s/%s/_status?pretty=true", esURL, repoName, name)
	res, err := client.Get(url)
	if err != nil {
		return 0, 0, err
	}
	defer res.Body.Close()

	statsJSON, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return 0, 0, err
	}
	var aggStats EsAggSnapshotStats
	err = json.Unmarshal(statsJSON, &aggStats)
	if err != nil {
		return 0, 0, errors.Wrapf(err, "Failed to parse es snapshot status as json data: \n\n%s\n\n", statsJSON)
	}
	if len(aggStats.Snapshots) < 1 {
		return 0, 0, errors.Errorf("No snapshot status data returned by Elasticsearch - response data: \n\n%s\n\n", statsJSON)
	}
	// We only ever request status for exactly 1 snapshot
	stats := aggStats.Snapshots[0]

	switch stats.State {
	// "STARTED" vs. "IN_PROGRESS" seems to be an es5 (and lower) vs. es6 change
	case "STARTED", "IN_PROGRESS", "SUCCESS":
		return stats.ShardsStats.Done, stats.ShardsStats.Total, nil
	default:
		e := errors.Errorf("elasticsearch snapshot '%s' failed with status '%s'", name, stats.State)
		return 0, 0, e
	}
}

// EngageMaintMode puts a1 into maintenance mode by creating the MaintDotFlag
// file.
func EngageMaintMode() error {
	f, err := os.OpenFile(MaintDotFlag, os.O_RDWR|os.O_CREATE, 0755)
	if err != nil {
		return err
	}
	return f.Close()
}

// verifyURL is a URL that we want to verify returns the given
// expected return code.
type verifyURL struct {
	url            string
	expectedReturn int
}

// VerifyMaintMode makes one or more HTTP requests to the A1/Chef server endpoints
// to ensure that they're not accepting data. Verification succeeds if the requests
// return 503; any other response is considered a failure. If a check fails after
// five tries the last error encountered is returned.
func VerifyMaintMode(conf *deployment.AutomateConfig) error {
	verifyURLs := []verifyURL{}
	client := insecureHTTPSClient()
	externalFqdn := conf.GetLoadBalancer().GetV1().GetSys().GetService().GetExternalFqdn().GetValue()
	externalPort := ""

	if port := conf.GetLoadBalancer().GetV1().GetSys().GetService().GetHttpsPort().GetValue(); port != 443 {
		externalPort = fmt.Sprintf(":%d", port)
	}

	externalFqdn = fmt.Sprintf("https://%s%s", externalFqdn, externalPort)

	a1DataCollectorURL := externalFqdn + "/data-collector/v0/"
	verifyURLs = append(verifyURLs, verifyURL{
		url:            a1DataCollectorURL,
		expectedReturn: 503,
	})

	if conf.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue() {
		// Verify that the Chef server is maintenance mode by accessing a URL
		// that should always be proxied to the Chef Server. In this instance
		// the required_recipe endpoint of a random org should suffice. If the
		// Chef server is in maintenance mode it should return a 503. If it is
		// not it should return either a 401 or 404, depending if the feature
		// is enabled or not. Since endpoints in the Chef server are scoped
		// to an org we'll use a random org name. It should not matter the
		// request should be matched and rejected in the Chef server nginx.
		csRequiredRecipeURL := externalFqdn + "/organizations/foo/required_recipe"
		verifyURLs = append(verifyURLs, verifyURL{
			url:            csRequiredRecipeURL,
			expectedReturn: 502,
		})
	}

	for _, url := range verifyURLs {
		var attempts uint
		for {
			err := verifyMaintModeURL(client, url)

			if err == nil {
				break
			}

			if attempts >= 5 {
				return err
			}

			time.Sleep((1 << attempts) * time.Second)
			attempts++
		}
	}

	return nil
}

func verifyMaintModeURL(client *http.Client, url verifyURL) error {
	res, err := client.Get(url.url)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	switch res.StatusCode {
	case url.expectedReturn:
		return nil
	default:
		return errors.Errorf(
			"Call to URL (%s) returned unexpected status (%s) -- expected a %d because Chef Automate v1 should be in maintenance mode",
			url.url, res.Status, url.expectedReturn)
	}
}

// VerifyA1QueueDrained checks A1's rabbitmq to ensure all queued data from the
// data collector gets written to disk before we continue with the upgrade. It
// checks the rabbitmq management API every 10s for up to 10m for a queue depth
// of zero. If the queue depth is still not zero after 10m, it will return the
// last error it encountered while checking.
func VerifyA1QueueDrained(a1Config *A1Config) error {

	pw := a1Config.DeliverySecrets.RabbitMQ.ManagementPassword
	client := insecureHTTPSClient()

	var attempts uint

	// Wait up to 10 minutes for the queue to drain, except when we're demoing
	// the UI for the failure case
	sleepTime := 10 * time.Second
	if Failure(MaintModeQueuedDataNotProcessed) {
		sleepTime = 1 * time.Second
	}

	for {
		err := testRabbitQueueIsZero(client, rabbitStatsURL, pw)
		if err != nil {
			if attempts >= 60 {
				return err
			}
			time.Sleep(sleepTime)
			attempts++
		} else {
			return nil
		}
	}
}

func testRabbitQueueIsZero(client *http.Client, url, rabbitMgmtPw string) error {
	req, _ := http.NewRequest("GET", url, nil)
	req.SetBasicAuth("rabbitmgmt", rabbitMgmtPw)
	res, err := client.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	statsJSON, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return err
	}
	var stats RabbitStats
	err = json.Unmarshal(statsJSON, &stats)
	if err != nil {
		return errors.Wrapf(err, "Failed to parse json data: \n\n%s\n\n", statsJSON)
	}
	if stats.Messages != 0 {
		return errors.Errorf("Rabbitmq stats from %s shows %d messages in the queue, want 0 messages in queue", rabbitStatsURL, stats.Messages)
	}

	return nil
}

func insecureHTTPSClient() *http.Client {
	tlsConfig := &tls.Config{InsecureSkipVerify: true}
	tr := &http.Transport{TLSClientConfig: tlsConfig}
	return &http.Client{Transport: tr, Timeout: 10 * time.Second}
}

// CheckSAMLConfig checks for SAML config in the Automate 1 PostgreSQL database
// and returns true if it is found
func CheckSAMLConfig(connInfo *pg.A1ConnInfo) (bool, error) {
	conn, err := pg.Connect(connInfo, "delivery")
	if err != nil {
		return false, errors.Wrap(err, "failed to connect to delivery database")
	}

	defer conn.Close()
	return conn.BoolQuery(SAMLConfigSQL)
}

// SpaceRequiredToMove attempts to determine how much space would be
// required on dst if `mv src dst` was called for each of the sources,
// assuming rename() will be called if src and dst are on the same
// filesystem.
func SpaceRequiredToMove(srcPath string, dstPath string) (uint64, error) {
	sameFs, err := IsSameFilesystem(srcPath, dstPath)
	if err != nil {
		return uint64(0), err
	}

	if sameFs {
		return uint64(0), nil
	}

	spaceRequired, err := SpaceUsedByDir(srcPath)
	if err != nil {
		return uint64(0), err
	}
	return spaceRequired, nil
}

// SpaceUsedByDir returns the disk space used in KB for the given
// directory.
func SpaceUsedByDir(path string) (uint64, error) {
	// du POSIX options:
	//
	// http://pubs.opengroup.org/onlinepubs/009695399/utilities/du.html
	//
	// -s: summarize (display total)
	// -k: block size 1024
	//
	output, err := defaultCommandExecutor.Output("du",
		command.Args("-s", "-k", path),
		command.Timeout(duTimeout))
	if err != nil {
		return 0, errors.Wrap(err, "du failed")
	}

	r := bufio.NewReader(strings.NewReader(output))
	l, err := r.ReadString('\n')
	if err != nil {
		return 0, errors.Wrap(err, "failed to read du output")
	}

	fields := strings.Fields(l)
	if len(fields) == 0 {
		return 0, errors.Wrap(err, "unable to parse du output")
	}

	kbAvailStr := fields[0]
	ret, err := strconv.ParseUint(kbAvailStr, 10, 64)
	if err != nil {
		return 0, errors.Wrap(err, "failed to convert du output to uint")
	}

	return ret, nil
}
