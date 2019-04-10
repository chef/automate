package a1upgrade

import (
	"fmt"
	"os"
	"strconv"
)

/*
initialConditions represents the idempotence scenarios we wish to demonstrate
in this prototype.
*/
type initialCondition int

const (
	// No upgrade steps are completed yet
	defaultCondition initialCondition = 0 // nolint: deadcode,varcheck

	// Case where we have a user-provided config file or we have one that we
	// generated previously
	configExists initialCondition = 100 // nolint: deadcode,varcheck

	// Extracted a1 data is present on disk, but a1 is fully up
	// Maybe we make sure we rename the a1 export file in our Failure cases, then
	// we don't worry about this case any more?
	a1DataExtracteda1Up initialCondition = 200 // nolint: deadcode,varcheck
	// Extracted a1 data is present on disk and a1 is down
	a1DataExtracteda1Down initialCondition = 201 // nolint: deadcode,varcheck
)

var requestedScenario initialCondition

// FailureScenario represents a set of failure conditions that we want to be
// able to test and demo. To invoke a given failure condition in an e2e test,
// set FAILURE=number on the command line. To check if the user has requested a
// certain failure scenario, use the Failure() function.
type FailureScenario int

const (
	// NoFailure indicates that the user hasn't asked to exercise a specific
	// failure case
	NoFailure FailureScenario = 0

	// PreflightNoA1 is the case we're running upgrade on a host that doesn't
	// have automate v1 installed.
	PreflightNoA1 FailureScenario = 100

	// PreflightA1TooOld is the case that the installed automate v1 doesn't meet
	// our minimum version requirement for upgrading
	PreflightA1TooOld FailureScenario = 101

	// PreflightA1Down is the case that automate v1 is not running. We need a1 to
	// be up so we can export data from postgres
	PreflightA1Down FailureScenario = 102

	// PreflightA1Unhealthy is the case that a1's status check is failing. We
	// just don't want to deal with all the ways that could possibly break
	// upgrades.
	PreflightA1Unhealthy FailureScenario = 103

	// Makes the database checker return 1TB as the size of every postgres
	// database, which will make the preflight fail unless you have a few TB of
	// disk.
	PreflightPostgresTooBig FailureScenario = 104

	// ConfigConflictFIPS is the case that a1 is configured for FIPS, which we
	// don't support currently
	ConfigConflictFIPS FailureScenario = 200

	// ConfigConflictNoBackup is the case that a1 doesn't have backups
	// configured. We want to very strongly encourage users to backup before
	// upgrading in case the upgrade doesn't work.
	ConfigConflictNoBackup FailureScenario = 201

	// ConfigConflictSAML is the case that a1 is configured with SAML, which we
	// don't migrate currently
	ConfigConflictSAML FailureScenario = 202

	// MaintModeDataCollectorNotDown is the case that we attempted to put a1 in
	// maintenance mode but the data collector URL isn't serving 503s
	MaintModeDataCollectorNotDown FailureScenario = 300

	// MaintModeDataCollectorRetryTest indicates that the test harness should
	// fail to return a 503 for the data collector URL a few times before
	// returning a 503
	MaintModeDataCollectorRetryTest FailureScenario = 302

	// MaintModeQueuedDataNotProcessed is the case that we put a1 in maintenance
	// mode but the data collector queue didn't fully drain within some timeout
	// period.
	MaintModeQueuedDataNotProcessed FailureScenario = 301

	// MaintModeQueuedDataRetryTest indicates that the test harness should return
	// a non-zero queue depth from the stub rabbitmq management API a few times
	// before returning a zero queue depth.
	MaintModeQueuedDataRetryTest FailureScenario = 303

	// MaintModeRequiredRecipeNotDown is the case that we attempted to put A1 in
	// maintenance mode but the Chef server isn't serving 503s
	MaintModeRequiredRecipeNotDown FailureScenario = 304

	// MaintModeRequiredRecipeRetryTest will fail to return a 503 for the
	// required_recipe URL a few times before returning a 503
	MaintModeRequiredRecipeRetryTest FailureScenario = 305

	// A1BackupFail is the case that the backup of a1 failed
	A1BackupFail FailureScenario = 400

	// ExtractA1DataFail is the case that we failed for some reason to dump a1's
	// postgres or copy some of the other data.
	ExtractA1DataFail FailureScenario = 500

	// A1ShutdownFail is the case that `automate-ctl stop` errored out for some
	// reason.
	A1ShutdownFail FailureScenario = 600

	// ChefServerShutdownFail is the case that `chef-server-ctl stop` errored out
	// for some reason
	ChefServerShutdownFail FailureScenario = 601

	// ChefServerShowSecretFail is the case that `chef-server-ctl
	// show-secret` errored out for some reason
	ChefServerShowSecretFail FailureScenario = 602

	// DataImportFailPostgres is the case that we couldn't load a1's pg data into
	// a2's pg
	DataImportFailPostgres FailureScenario = 700

	// DataImportFailElastic is the case that we couldn't load a1's es data into
	// a2's es
	DataImportFailElastic FailureScenario = 701

	// Stage1MigrationFail is the case that domain services failed to ingest the
	// staged a1 data for some reason (or otherwise failed to start)
	Stage1MigrationFail FailureScenario = 800
)

var requestedFailure FailureScenario
var FailureInt int

func init() {
	scenarioValue, scenarioSet := os.LookupEnv("SCENARIO")
	if scenarioSet {
		var scenarioInt int
		_, err := fmt.Sscanf(scenarioValue, "%d", &scenarioInt)
		if err != nil {
			msg := fmt.Sprintf("failed to parse SCENARIO env var to int (value: %s): %v\n", scenarioValue, err)
			panic(msg)
		}
		requestedScenario = initialCondition(scenarioInt)
	}

	FailureValue, FailureSet := os.LookupEnv("FAILURE")
	if FailureSet && FailureValue != "" {
		var err error
		FailureInt, err = strconv.Atoi(FailureValue)
		if err != nil {
			msg := fmt.Sprintf("FAILURE environment variable '%s' must be an integer", FailureValue)
			panic(msg)
		}
		requestedFailure = FailureScenario(FailureInt)
	}
}

func condition(maybeCondition initialCondition) bool { // nolint: deadcode
	return maybeCondition == requestedScenario
}

// Failure determines if the given maybeFailure FailureScenario was set by the
// user. EX:
// if Failure(A1ShutdownFail) {
//   return aCannedError()
// }
// // normal program execution...
func Failure(maybeFailure FailureScenario) bool {
	return maybeFailure == requestedFailure
}
