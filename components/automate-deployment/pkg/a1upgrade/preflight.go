package a1upgrade

import (
	"bytes"
	"crypto/tls"
	"fmt"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/platform/sys"
)

var (
	errPreflightCheckFailed = errors.New("upgrade preflight checks failed")
)

// A1StatusURL is the HTTP URL for the A1 Status API endpoint
var A1StatusURL = "https://localhost/api/_status"

const (
	// A1RequiredMajorVersion is major component of the minimum
	// allowed Automate v1 version that we will upgrade from.
	A1RequiredMajorVersion = 1
	// A1RequiredMinorVersion is minor component of the minimum
	// allowed Automate v1 version that we will upgrade from.
	A1RequiredMinorVersion = 8
	// A1RequiredPatchVersion is patch component of the minimum
	// allowed Automate v1 version that we will upgrade from.
	A1RequiredPatchVersion = 38
)

const freeDiskSafetyMargin = 1.15

// The relationship between the size of a database in postgres and the size of
// the output of pg_dump is enormously variable. The main factors seem to be 1)
// large pieces of data stored in postgres are generally compressed, but are
// uncompressed in pg_dump output, resulting in much larger sizes; and 2) the
// pg_dump output doesn't have indexes, which can make the pg_dump output
// smaller than the disk used by postgres. In limited testing, we have observed
// that the delivery database for automate.chef.co expands from 4GB to 24GB
// when dumped, while the opscode_chef database on chef-server.chef.co takes up
// 128MB in postgres but emits only an 88MB pg_dump.
//
// We are erring on the side of caution here as the largest databases are
// likely to consist of a lot of compressed data (e.g., cookbook files stored
// by bookshelf in pg mode, workflow job logs, etc.)
const pgDumpSafetyMargin = 8

var chefServerDatabaseNames = []string{
	"opscode_chef",
	"bifrost",
	"oc_id",
	"bookshelf",
}

const pgDbSizeQuery = `SELECT pg_catalog.pg_database_size(d.datname) AS SIZE
FROM pg_catalog.pg_database d
WHERE d.datname = '%s';`

// The PreflightRunner aggregates failures across our upgrade
// preflight checks and presents them to the user once all checks have
// been run.
type PreflightRunner struct {
	deliveryRunning *DeliveryRunning
	deliverySecrets *DeliverySecrets
	reindexer       *Reindexer
	writer          cli.FormatWriter
	checkChefServer bool
	checkWorkflow   bool
	details         []string
	err             error
}

// NewPreflightRunner sets up a PreflightRunner
func NewPreflightRunner(
	writer cli.FormatWriter,
	deliveryRunning *DeliveryRunning,
	deliverySecrets *DeliverySecrets,
	checkChefServer bool,
	checkWorkflow bool) *PreflightRunner {

	return &PreflightRunner{
		writer:          writer,
		deliveryRunning: deliveryRunning,
		deliverySecrets: deliverySecrets,
		checkChefServer: checkChefServer,
		checkWorkflow:   checkWorkflow,
	}
}

// Run the preflight checks
func (p *PreflightRunner) Run() error {
	// Note: the deploy preflight checks use cli.FormatWriter.Title() for both
	// the informational header and the output of the preflight shell script,
	// which results in two newlines between the header and the output of the
	// checks. Since we write output differently here, we need an extra newline
	// to make the UI match.
	p.writer.Title("Beginning pre-flight checks for upgrade\n")
	p.checkA1Installed()
	p.checkA1MinimumVersion()
	p.checkA1InstallUp()
	p.checkA1InstallHealthy()
	p.checkExpectedDataPaths()
	p.checkPGPermissions()
	p.checkFreeSpace()
	p.checkFreeSpaceForEs2Reindex()
	p.checkForUnknownEs2Indices()

	if p.err != nil {
		p.printDetails()
	}
	return p.err
}

func (p *PreflightRunner) printDetails() {
	for _, msg := range p.details {
		p.writer.Printf("\n")
		p.writer.Println(msg)
	}
}

func (p *PreflightRunner) printUnknown(desc string) {
	p.writer.Printf("UNKN|  %s\n", desc)
}

func (p *PreflightRunner) printOk(desc string) {
	p.writer.Printf(" OK |  %s\n", desc)
}

func (p *PreflightRunner) printFail(desc string) {
	p.writer.Printf("FAIL|  %s\n", desc)
}

func (p *PreflightRunner) addFailure(checkDesc string, msg string, err error) {
	p.printFail(checkDesc)
	p.err = err
	p.details = append(p.details, msg)
}

func (p *PreflightRunner) checkA1Installed() {
	ok, err := fileutils.PathExists(A1VersionManifestPath)
	if err != nil {
		p.err = errors.Wrap(err, "error looking for Automate 1 version manifest")
		p.printUnknown("Could not determine Automate 1 installation state")
		return
	}

	if !ok {
		explanation := `No Chef Automate v1 install was detected on this system. To install Chef
Automate v2, run:

    chef-automate deploy
`
		p.addFailure("did not detect Chef Automate v1 install", explanation, errPreflightCheckFailed)
		return
	}

	p.printOk("detected Chef Automate v1 install")
}

func (p *PreflightRunner) checkA1MinimumVersion() {
	if p.err != nil {
		return
	}

	checkDesc := fmt.Sprintf("existing install is version %d.%d.%d or greater",
		A1RequiredMajorVersion,
		A1RequiredMinorVersion,
		A1RequiredPatchVersion)

	versionStr, err := VersionStringFromA1Manifest()
	if err != nil {
		p.err = errors.Wrap(err, "error reading Automate 1 version manifest")
		p.printUnknown("Could not determine Automate 1 version")
		return
	}

	parseError := func(e error) {
		p.err = errors.Wrap(e, "error parsing Automate 1 version")
		detail := fmt.Sprintf(`Could not parse Automate 1 version information.
Attempting to parse the version data:

    %s

failed with:

   %v
`, versionStr, e)
		p.details = append(p.details, detail)
		p.printUnknown("Could not determine Automate 1 version")
	}

	parts := strings.Split(versionStr, ".")

	if len(parts) < 3 {
		parseError(errors.New("expected version in X.Y.Z format"))
		return
	}

	major, err := strconv.Atoi(parts[0])
	if err != nil {
		parseError(err)
		return
	}

	minor, err := strconv.Atoi(parts[1])
	if err != nil {
		parseError(err)
		return
	}

	patch, err := strconv.Atoi(parts[2])
	if err != nil {
		parseError(err)
		return
	}

	details := fmt.Sprintf(`Upgrade is only supported from Chef Automate %d.%d.%d or later.
Your Automate v1 installation is at version %d.%d.%d.

Please update your Chef Automate v1 installation to this version and then try again.
`, A1RequiredMajorVersion, A1RequiredMinorVersion, A1RequiredPatchVersion, major, minor, patch)

	if major < A1RequiredMajorVersion {
		p.addFailure(checkDesc, details, errPreflightCheckFailed)
		return
	}

	if major == A1RequiredMajorVersion && minor < A1RequiredMinorVersion {
		p.addFailure(checkDesc, details, errPreflightCheckFailed)
		return
	}

	if major == A1RequiredMajorVersion && minor == A1RequiredMinorVersion && patch < A1RequiredPatchVersion {
		p.addFailure(checkDesc, details, errPreflightCheckFailed)
		return
	}

	p.printOk(checkDesc)
}

func (p *PreflightRunner) checkA1InstallUp() {
	if p.err != nil {
		return
	}

	err := AutomateCtlStatus()
	if err != nil {
		details := `Chef Automate v1 is not currently running. Chef Automate v1 must be up
and healthy when starting the upgrade for data to be correctly
migrated. Run the following command to start Chef Automate v1, then
try again:

    automate-ctl start
`
		p.addFailure("existing install is down", details, errPreflightCheckFailed)
		return
	}

	p.printOk("existing install is up")
}

func (p *PreflightRunner) checkA1InstallHealthy() {
	if p.err != nil {
		return
	}

	tlsConfig := &tls.Config{InsecureSkipVerify: true}
	tr := &http.Transport{TLSClientConfig: tlsConfig}
	c := &http.Client{Transport: tr, Timeout: 10 * time.Second}
	resp, err := c.Get(A1StatusURL)
	if err != nil || resp.StatusCode != 200 {
		details := `Chef Automate v1 is reporting status errors and may not be suitable for
upgrade. Please remediate these issues such that

    automate-ctl status

returns 0 and the status endpoint:

    https://localhost/api/_status

returns HTTP 200 OK.

If you are unable to diagnose or fix the problem, please contact
support@chef.io for assistance.
`
		p.addFailure("existing install reports unhealthy status", details, errPreflightCheckFailed)
		return
	}

	p.printOk("existing install is healthy")
}

// checkExpectedDataPaths checks the source and destination paths for
// the transfers.
//
// - Source Path must exist unless the migration has been completed.
// - Destination Path must not exist or be an empty directory (or
//   symlink to an empty directory)
//
func (p *PreflightRunner) checkExpectedDataPaths() {
	if p.err != nil {
		return
	}

	checkDesc := "expected Automate 1 data paths exist"
	moveJobs := FileMoversForConfig(p.deliveryRunning, p.checkWorkflow)

	missingPaths := make([]string, 0)
	for _, m := range moveJobs {

		destExists, err := fileutils.PathExists(m.DestPath())
		if err != nil {
			p.err = errors.Wrap(err, "stat on expected data directory failed")
			p.printUnknown(checkDesc)
			return
		}

		destIsEmptyDir, err := IsEmptyDir(m.DestPath())
		if err != nil {
			p.err = errors.Wrap(err, "stat on expected data directory failed")
			p.printUnknown(checkDesc)
			return
		}

		moveDone, err := m.AlreadyMoved()
		if err != nil {
			p.err = errors.Wrapf(err, "unable to determine migration status of %s", m.DestPath())
			p.printUnknown(checkDesc)
			return
		}

		moveStarted, err := m.MoveStarted()
		if err != nil {
			p.err = errors.Wrapf(err, "unable to determine migration status of %s", m.DestPath())
			p.printUnknown(checkDesc)
			return
		}

		// If we've already done the migration we skip any
		// source-dir check
		okToExist := moveStarted || moveDone || destIsEmptyDir
		if destExists && !okToExist {
			explanation := fmt.Sprintf(`The directory

    %s

already exists and is non-empty. This path is part of Automate 2 and
will be created by the upgrade process.

Please remove the directory and re-run the upgrade.
`, m.DestPath())
			p.addFailure(checkDesc, explanation, errPreflightCheckFailed)
			return
		}

		// Check source directory. It should exist
		stat, err := os.Stat(m.SrcPath)
		if os.IsNotExist(err) || (err == nil && !stat.IsDir()) {
			missingPaths = append(missingPaths, m.SrcPath)
			continue
		}

		if err != nil {
			p.err = errors.Wrap(err, "stat on expected data directory failed")
			p.printUnknown(checkDesc)
			return
		}
	}

	if len(missingPaths) > 0 {
		buf := &bytes.Buffer{}
		fmt.Fprintln(buf, "Missing expected data paths:")
		for _, p := range missingPaths {
			fmt.Fprintf(buf, "  %s\n", p)
		}
		fmt.Fprintf(buf, "If you are running this after a previously failed upgrade, run the upgrade with --skip-preflight")
		p.addFailure(checkDesc, buf.String(), errPreflightCheckFailed)
		return
	}

	p.printOk(checkDesc)
}

type pathMove struct {
	SrcPath        string
	DestMountPoint string
}

type moveJob interface {
	RequiredSpaceWithSafetyMargin(*PreflightRunner) (uint64, error)
}

func (p pathMove) RequiredSpaceWithSafetyMargin(_ *PreflightRunner) (uint64, error) {
	spaceRequired, err := SpaceRequiredToMove(p.SrcPath, p.DestMountPoint)
	if err != nil {
		return uint64(0), err
	}

	spaceRequired = uint64(float64(spaceRequired) * freeDiskSafetyMargin)
	return spaceRequired, nil
}

type pgMove struct {
	Databases []string
}

func (pgMoveJob pgMove) RequiredSpaceWithSafetyMargin(p *PreflightRunner) (uint64, error) {
	// skip unneeded db connection if there's nothing to check
	if len(pgMoveJob.Databases) == 0 {
		return uint64(0), nil
	}

	if Failure(PreflightPostgresTooBig) {
		// return value is in kilobytes
		terabyte := uint64(1) << (30)
		fmt.Printf("PreflightPostgresTooBig failure scenario invoked, setting data required size to %dkb\n", terabyte)
		return terabyte, nil
	}

	db, err := p.connectToPostgres()
	if err != nil {
		return uint64(0), err
	}
	defer db.Close()

	var totalDBBytes int64

	for _, dbname := range pgMoveJob.Databases {
		res, err := db.BigintQuery(fmt.Sprintf(pgDbSizeQuery, dbname))
		if err != nil {
			e := errors.Wrapf(err, "failed to query size of postgres database %s", dbname)
			return uint64(0), e
		}
		totalDBBytes += res
	}

	requiredFreeSpaceBytes := uint64(totalDBBytes * pgDumpSafetyMargin)
	requiredFreeSpaceKB := requiredFreeSpaceBytes / 1024

	return requiredFreeSpaceKB, nil
}

func (p *PreflightRunner) spaceRequiredForAllMoveJobs(jobs []moveJob) (uint64, error) {
	totalSpace := uint64(0)
	for _, job := range jobs {
		spaceForJob, err := job.RequiredSpaceWithSafetyMargin(p)
		if err != nil {
			return uint64(0), err
		}
		totalSpace += spaceForJob
	}
	return totalSpace, nil
}

func (p *PreflightRunner) pgDatabasesToCheck() []string {
	databases := []string{}
	if p.checkChefServer {
		databases = append(databases, chefServerDatabaseNames...)
	}
	if p.checkWorkflow {
		databases = append(databases, "delivery")
	}
	return databases
}

func (p *PreflightRunner) checkFreeSpace() {
	if p.err != nil {
		return
	}

	checkDesc := "sufficient disk space to move Automate 1 data"
	moveJobsByMount := make(map[string][]moveJob)
	moveJobs := FileMoversForConfig(p.deliveryRunning, p.checkWorkflow)
	for _, m := range moveJobs {
		mountPoint, err := sys.MountFor(m.DestPath())
		if err != nil {
			p.printUnknown(checkDesc)
			p.err = errors.Wrap(err, "disk space check failed")
			return
		}
		moveJobsByMount[mountPoint] = append(moveJobsByMount[mountPoint], pathMove{DestMountPoint: mountPoint, SrcPath: m.SrcPath})
	}

	pgDumpMountPoint, err := sys.MountFor(PGDumpOutputDir)
	if err != nil {
		p.printUnknown(checkDesc)
		p.err = errors.Wrapf(err, "failed to determine mount point for postgres data staging dir %s", PGDumpOutputDir)
		return
	}
	pgMoveJob := pgMove{Databases: p.pgDatabasesToCheck()}
	moveJobsByMount[pgDumpMountPoint] = append(moveJobsByMount[pgDumpMountPoint], pgMoveJob)

	for destMountPoint, moveJobs := range moveJobsByMount {
		spaceRequired, err := p.spaceRequiredForAllMoveJobs(moveJobs)
		if err != nil {
			p.printUnknown(checkDesc)
			p.err = errors.Wrap(err, "disk space check failed")
			return
		}
		spaceAvailable, err := sys.SpaceAvailForPath(destMountPoint)
		if err != nil {
			p.printUnknown(checkDesc)
			p.err = errors.Wrap(err, "disk space check failed")
			return
		}

		if spaceAvailable < spaceRequired {
			msg := fmt.Sprintf("Insufficient disk space on %s for migration.\n %18s: %7d MB\n %18s: %7d MB\n",
				destMountPoint,
				"Space Required", spaceRequired/1024,
				"Space Available", spaceAvailable/1024)
			p.addFailure(checkDesc, msg, errPreflightCheckFailed)
			return
		}
	}

	p.printOk(checkDesc)
}

func (p *PreflightRunner) getReindexer() (*Reindexer, error) {
	if p.reindexer != nil {
		return p.reindexer, nil
	}
	esURL := p.deliveryRunning.Delivery.Elasticsearch.NginxProxyURL
	r, err := NewReindexer(p.writer, esURL)
	if err != nil {
		return nil, err
	}
	p.reindexer = r
	return p.reindexer, nil
}

func (p *PreflightRunner) checkFreeSpaceForEs2Reindex() {
	if p.err != nil {
		return
	}

	checkDesc := "sufficient disk space to reindex Elasticsearch 2 data"
	r, err := p.getReindexer()
	if err != nil {
		p.addFailure(checkDesc, "cannot initialize connection to Automate 1 Elasticsearch instance", err)
		return
	}
	requiredBytes, err := r.DiskRequirementsInBytes()
	if err != nil {
		p.addFailure(checkDesc, "failed querying Elasticsearch for index format and disk space", err)
		return
	}
	if requiredBytes == int64(0) {
		return
	}

	// platform.SpaceAvailForPath returns kilobytes
	spaceRequiredWithoutMargin := float64(requiredBytes / 1024)

	a1EsDataPath := p.deliveryRunning.Delivery.Insights.DataDirectory
	a1EsMountPath, err := sys.MountFor(a1EsDataPath)
	if err != nil {
		p.printUnknown(checkDesc)
		p.err = errors.Wrap(err, "disk space check failed")
		return
	}

	spaceRequired := uint64(spaceRequiredWithoutMargin * freeDiskSafetyMargin)
	spaceAvailable, err := sys.SpaceAvailForPath(a1EsMountPath)
	if err != nil {
		p.printUnknown(checkDesc)
		p.err = errors.Wrapf(err, "disk space check on mount point '%s' failed", a1EsMountPath)
		return
	}

	if spaceAvailable < spaceRequired {
		msg := fmt.Sprintf("Insufficient disk space on %s for migration.\n %18s: %7d MB\n %18s: %7d MB\n",
			a1EsMountPath,
			"Space Required", spaceRequired/1024,
			"Space Available", spaceAvailable/1024)
		p.addFailure(checkDesc, msg, errPreflightCheckFailed)
		return
	}
	p.printOk(checkDesc)
}

func (p *PreflightRunner) checkForUnknownEs2Indices() {
	if p.err != nil {
		return
	}

	checkDesc := "unknown Elasticsearch v2 indices not present"
	r, err := p.getReindexer()
	if err != nil {
		p.addFailure(checkDesc, "cannot initialize connection to Automate 1 Elasticsearch instance", err)
		return
	}
	err = r.UnknownIndicesError()
	if err != nil {
		p.addFailure(checkDesc, "Elasticsearch contains version 2 indices that cannot be migrated", err)
		return
	}
	p.printOk(checkDesc)
}

func (p *PreflightRunner) checkPGPermissions() {
	if p.err != nil {
		return
	}

	checkDesc := "configured Automate 1 postgresql user is a superuser"
	db, err := p.connectToPostgres()
	if err != nil {
		msg := makePgPermCheckFailDetail(err)
		p.addFailure(checkDesc, msg, err)
		return
	}
	defer db.Close()

	isSuper, err := db.ConnectedUserIsSuperuser()
	if err != nil {
		msg := makePgPermCheckFailDetail(err)
		p.addFailure(checkDesc, msg, err)
		return
	}
	if !isSuper {
		err := errors.New("configured PostgreSQL user does not have the SUPERUSER role")
		msg := makePgPermCheckFailDetail(err)
		p.addFailure(checkDesc, msg, err)
		return
	}

	p.printOk(checkDesc)
}

func (p *PreflightRunner) connectToPostgres() (pg.DB, error) {
	port, err := p.deliveryRunning.Delivery.PostgreSQL.Port.Int64()

	if err != nil {
		e := errors.Wrap(err, "cannot parse PostgreSQL port from Automate 1 configuration")
		return nil, e
	}

	connInfo := pg.A1ConnInfo{
		Pass: p.deliverySecrets.Postgresql.SuperuserPassword,
		User: p.deliveryRunning.Delivery.PostgreSQL.SuperuserUsername,
		Host: p.deliveryRunning.Delivery.PostgreSQL.Vip,
		Port: uint64(port),
	}

	db, err := pg.Connect(&connInfo, "template1")
	if err != nil {
		return nil, makePgConnectFailErr(&connInfo, err)
	}
	if err = db.Ping(); err != nil {
		return nil, makePgConnectFailErr(&connInfo, err)
	}
	return db, nil
}

func makePgConnectFailErr(connInfo *pg.A1ConnInfo, err error) error {
	buf := new(bytes.Buffer)
	fmt.Fprintln(buf, "Error connecting to the Chef Automate v1 Postgresql database")
	fmt.Fprintln(buf, "\nConnection information from the Chef Automate 1 configuration:")
	fmt.Fprintf(buf, "      Host: %s\n", connInfo.Host)
	fmt.Fprintf(buf, "      Port: %d\n", connInfo.Port)
	fmt.Fprintf(buf, "  Username: %s\n", connInfo.User)
	fmt.Fprintln(buf, "  Password: (read from postgresql.superuser_password in /etc/delivery/delivery-secrets.json)")
	return errors.Wrap(err, buf.String())
}

func makePgPermCheckFailDetail(err error) string {
	buf := new(bytes.Buffer)
	fmt.Fprintln(buf, "Chef Automate 1 PostgreSQL user permissions could not be confirmed.")
	fmt.Fprintln(buf, "The migration process requires a PostgreSQL superuser.")
	fmt.Fprintln(buf, "\nChecking for the SUPERUSER role for this user failed with:")
	fmt.Fprintf(buf, "    %s\n", err.Error())
	return buf.String()
}
