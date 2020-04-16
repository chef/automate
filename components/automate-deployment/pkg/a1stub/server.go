package a1stub

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"regexp"
	"sort"
	"strings"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/assets"
	"github.com/chef/automate/lib/platform/pg"
)

var logger *logrus.Logger

var a1APIServer *http.Server

var esAPIServer *http.Server

var rabbitAPIServer *http.Server

var tlsCertPath string

var tlsKeyPath string

// Number of days of Es indices we generate
const daysToGenerate = 100

const reindexEs2ChunkSize = 100

const ChefServerNginxStoppedSentinel = "/var/opt/opscode/a1stub_stopped_nginx"

func setupCerts() error {
	err := assetTmpfile("ChefAutomateUpgradeFromv1SelfTest.crt", []byte(assets.ChefAutomateUpgradeFromV1SelfTestCRT), &tlsCertPath)
	if err != nil {
		return err
	}
	return assetTmpfile("ChefAutomateUpgradeFromv1SelfTest.key", []byte(assets.ChefAutomateUpgradeFromV1SelfTestKEY), &tlsKeyPath)
}

func cleanupCerts() {
	if tlsCertPath != "" {
		os.Remove(tlsCertPath)
	}
	if tlsKeyPath != "" {
		os.Remove(tlsKeyPath)
	}
}

func assetTmpfile(assetName string, content []byte, tmpPath *string) error {
	tmpfile, err := ioutil.TempFile("", assetName)
	if err != nil {
		return err
	}
	*tmpPath = tmpfile.Name()
	return ioutil.WriteFile(tmpfile.Name(), content, os.ModePerm)
}

func serveA1API() {
	logger = logrus.New()
	logger.Formatter = &logrus.JSONFormatter{}

	err := setupA1ApiServer().ListenAndServeTLS(tlsCertPath, tlsKeyPath)
	thPrintf("A1 server stopped, returned: %v\n", err)
}

func serveRabbitAPI() {
	logger = logrus.New()
	logger.Formatter = &logrus.JSONFormatter{}

	err := setupRabbitAPIServer().ListenAndServeTLS(tlsCertPath, tlsKeyPath)
	thPrintf("rabbit server stopped, returned: %v\n", err)
}

func serveESAPI() {
	logger = logrus.New()
	logger.Formatter = &logrus.JSONFormatter{}

	err := setupEsAPIServer().ListenAndServe()
	thPrintf("es server stopped, returned: %v\n", err)
}

type mockDBProvider struct{}
type mockDB struct{}

func (m mockDBProvider) Connect(c pg.ConnInfoURI, dbname string) (pg.DB, error) {
	thPrintf("PGMock Connect() to %s\n", c)
	return mockDB{}, nil
}

func (m mockDB) Close() error {
	thPrintf("PGMock Closed()\n")
	return nil
}

func (m mockDB) Ping() error {
	thPrintf("PGMock Ping()\n")
	return nil
}

func (m mockDB) CreateDatabase(name string) error {
	thPrintf("PGMock CreateDatabase(%s)\n", name)
	return nil
}
func (m mockDB) CreateDatabaseWithOwner(db string, user string) error {
	thPrintf("PGMock CreateDatabaseWithOwner(%s, %s)\n", db, user)
	return nil
}

func (m mockDB) CreateRole(user string) error {
	thPrintf("PGMock CreateRole(%s)\n", user)
	return nil
}

func (m mockDB) DropDatabase(name string) error {
	thPrintf("PGMock DropDatabase(%s)\n", name)
	return nil
}

func (m mockDB) ConnectedUserIsSuperuser() (bool, error) {
	thPrintf("PGMock ConnectedUserIsSuperuser()\n")
	return true, nil
}

func (m mockDB) DatabaseExists(name string) (bool, error) {
	thPrintf("PGMock DatabaseExists(%s)\n", name)
	return true, nil
}

func (m mockDB) RenameDatabase(old string, new string) error {
	thPrintf("PGMock RenameDatabase(%s, %s)\n", old, new)
	return nil
}

func (m mockDB) CreateExtension(extName string) error {
	thPrintf("PGMock CreateExtension(%s)\n", extName)
	return nil
}

func (m mockDB) GrantAll(dbName string, roleName string) error {
	thPrintf("PGMock GrantAll(%s, %s)\n", dbName, roleName)
	return nil
}

func (m mockDB) RemovePassword(roleName string) error {
	thPrintf("PGMock RemovePassword(%s)\n", roleName)
	return nil
}

func (m mockDB) ExecStatement(statement string, args ...interface{}) error {
	thPrintf("PGMock ExecStatement(%s, %v)\n", statement, args)
	return nil
}

func (m mockDB) StringQuery(query string, args ...interface{}) (string, error) {
	thPrintf("PGMock StringQuery(%s, %v)\n", query, args)
	switch query {
	case a1upgrade.UserRolesExportSQL:
		// Note: In the a1-migration setup, "alice" and "bob" have not been assigned
		// those roles; however, if they were, this is how the data file would look.
		return `[{"name":"builder","roles":["admin"]},
{"name":"admin","roles":["admin"]},
{"name":"alice","roles":["shipper","committer"]},
{"name":"bob","roles":["observer","reviewer"]},
{"name":"test-admin","roles":["admin"]}]`, nil

	case a1upgrade.UsersExportSQL:
		// Note: The passwords of alice, bob and test-admin are "${USER}-password"
		// the password of admin is "migrated-admin-password"
		// the password of builder is "migrated-builder-password"
		//
		// These hashes can be created on the command line using htpasswd:
		//
		// htpasswd -bnBC 12 "" migrated-builder-password | sed s/:\$2y/\$2a/
		//
		// The migrated_users controls of the a2-migrate-from-v1-integration inspec
		// suite depend on these passwords. The admin-password is made available for
		// inspec in integration/tests/[airgap_]a1migration.sh.
		return `[{"name":"admin","first_name":null,"last_name":null,
"hashed_pass":"$2a$12$SWA2q.A2Pe8PzNw.i7DcruoIoq.Lgvz0G7O07.V21I077PveSkGy6"},
{"name":"builder","first_name":null,"last_name":null,
"hashed_pass":"$2a$12$DpdJVmcRKtz8LFB3cZ.QrOWA.3XrbW6htKrzKl1xT4Z/XLGgFR90y"},
{"name":"test-admin","first_name":null,"last_name":null,
"hashed_pass":"$2a$12$PR6X8D1BoTZCWLePEOU87OQEGIGjMcwDWrjkb19jfHadGptXuVIT2"},
{"name":"bob","first_name":"Bob","last_name":"Bobbikowsky",
"hashed_pass":"$2a$12$iTU/IeXdF6glnD5tglEq3.qYaABj269kuAsI/IcgUOiZ6vV4mpc7e"},
{"name":"alice","first_name":"Alice","last_name":"Schmidt",
"hashed_pass":"$2a$12$qG7/7vIyzWq3xyxTCB2kV.TEZ7268hVIp60yGl8A9CAPBWEoQvm.S"}]`, nil
	}
	return "", nil
}

func (m mockDB) BoolQuery(query string, args ...interface{}) (bool, error) {
	thPrintf("PGMock BoolQuery(%s, %v)\n", query, args)
	return strings.Contains(query, a1upgrade.SAMLConfigSQL), nil
}

func (m mockDB) BigintQuery(query string, args ...interface{}) (int64, error) {
	thPrintf("PGMock BigintQuery(%s, %v)\n", query, args)
	return int64(0), nil
}

func (m mockDB) AlterDatabaseOwner(dbname string, owner string) error {
	thPrintf("PGMock AlterDatabaseOwner(%s, %s)\n", dbname, owner)
	return nil
}

func servePostgreSQLStub() {
	thPrintf("Installing mock PgDB provider\n")
	pg.CurrentDBProvider = mockDBProvider{}
}

func shutdownPostgreSQLStub() {
	thPrintf("Removing mock PgDB provider\n")
	pg.CurrentDBProvider = pg.DefaultDBProvider
}

func setupA1ApiServer() *http.Server {
	mux := http.NewServeMux()
	// Emulated A1 APIs:
	mux.HandleFunc("/data-collector/v0/", handleDataCollector)
	mux.HandleFunc("/api/_status/", handleStatus)
	// Chef server AIO APIs:
	mux.HandleFunc("/organizations/", handleChefServerReq)
	// test harness control functions to control the stub server
	mux.HandleFunc("/th-ctl/stop", thCtlStop)
	mux.HandleFunc("/th-ctl/createbackup", thCtlCreateBackup)

	a1APIServer = &http.Server{
		Addr:         ":443",
		Handler:      mux,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
		ErrorLog:     log.New(os.Stderr, "", 0),
	}
	thPrintf("Starting stub A1 api %v\n", a1APIServer)
	return a1APIServer
}

func setupRabbitAPIServer() *http.Server {
	rabbitAPIServer = &http.Server{
		Addr: ":15672",
		// the default http.ServeMux includes a lot of extra behavior that breaks
		// our stub rabbit API. in particular it seems to include a StripPrefix
		// that will just redirect our requests that have encoded slashes in them
		// ("%2f" == "/") to the "normalized" path which would make it impossible
		// for us to ever get requests with "%2f" in them, which we must do to
		// provide a correct stub of rabbit. So we send all requests to the this
		// function and check the URL inside of it.
		Handler:      http.HandlerFunc(handleRabbitStats),
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
		ErrorLog:     log.New(os.Stderr, "", 0),
	}
	thPrintf("Starting stub rabbit api %v\n", rabbitAPIServer)
	return rabbitAPIServer
}

func setupEsAPIServer() *http.Server {
	mux := http.NewServeMux()
	// We actually are emulating the internal load balancer because
	// migrate-from-v1 talks to that instead of hitting es directly
	// therefore we have /elasticsearch/ prepended to everything.
	// Emulated ES APIs:
	// - snapshot status API: note that we set backup type to fs in the test
	// harness because that enables some extra preflight checks. That means the
	// snapshot repository will be named fs-chef-automate
	mux.HandleFunc("/elasticsearch/_snapshot/fs-chef-automate/", handleEsSnapshotStatus)

	// These functions get metadata about Es indices. They're used when the
	// upgrade command checks for Es2 indices.
	mux.HandleFunc("/elasticsearch/_all/_settings", handleEsIndicesSettings)

	for _, path := range expectedEsIndicesSettingsURIPaths() {
		// ServeMux converts %2C -> ',' prior to matching the handler func
		mux.HandleFunc(path, handleEsIndicesSettings)
	}

	// We expect these indices to get created as part of the es2 reindex process:
	for _, indexName := range insightsIndicesNames() {
		path := fmt.Sprintf("/elasticsearch/%s-1", indexName)
		mux.HandleFunc(path, handleEsIndexCreate)
	}

	for _, indexName := range complianceIndicesNames() {
		path := fmt.Sprintf("/elasticsearch/%s-1", indexName)
		mux.HandleFunc(path, handleEsIndexCreate)
	}

	// We expect indices to get reindexed from old to new indices for the es2 reindex:
	mux.HandleFunc("/elasticsearch/_reindex", handleEsReindex)

	// We expect these indices to get deleted as part of the es2 reindex process:
	for _, indexName := range insightsIndicesNames() {
		path := fmt.Sprintf("/elasticsearch/%s", indexName)
		mux.HandleFunc(path, handleEsIndexDelete)
	}

	for _, indexName := range complianceIndicesNames() {
		path := fmt.Sprintf("/elasticsearch/%s", indexName)
		mux.HandleFunc(path, handleEsIndexDelete)
	}

	// These are also deleted, but they are just deleted, not reindexed
	mux.HandleFunc("/elasticsearch/.automate", handleEsIndexDelete)
	mux.HandleFunc("/elasticsearch/saved-searches", handleEsIndexDelete)

	// We expect compliance indices to have an alias created:
	mux.HandleFunc("/elasticsearch/_aliases", handleEsAlias)

	for _, path := range expectedEsStatsStoreURIPaths() {
		// preflight checks will query for estimated disk-space requirements:
		mux.HandleFunc(path, handleEsIndicesStatsStore)
	}

	mux.HandleFunc("/elasticsearch/_tasks/", handleEsTaskStatus)

	mux.HandleFunc("/", debug404Handler)

	esAPIServer = &http.Server{
		Addr:         ":8080",
		Handler:      mux,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
		ErrorLog:     log.New(os.Stderr, "", 0),
	}
	thPrintf("Starting stub elasticsearch api %v\n", esAPIServer)
	return esAPIServer

}

func debug404Handler(w http.ResponseWriter, r *http.Request) {
	thPrintf("Received non-matching request %v\n", r)
	panic("oops")
}

var dcRetryTestCount = 0
var rrRetryTestCount = 0

// Stub version of a1 API at /data-collector/v0/
// Only returns 401 or 503.
// If the maintModeFile doesn't exist, then we emulate a1 being up and
// accepting requests by returning a 401, which the upgrader will expect since
// it doesn't provide a valid auth token
// If maintModeFile exists, then we emulate maintenance mode by returning a 503
func handleDataCollector(w http.ResponseWriter, r *http.Request) {
	thPrintf("stub API received request: %v\n", r)

	if a1upgrade.Failure(a1upgrade.MaintModeDataCollectorRetryTest) && dcRetryTestCount <= 4 {
		thPrintf("simulating maintenance mode temporary failure %d/5\n", dcRetryTestCount)
		dcRetryTestCount++
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprintln(w, "emulated response: 401 FORBIDDEN")
		return
	}

	if a1upgrade.Failure(a1upgrade.MaintModeDataCollectorNotDown) {
		thPrint("simulating maintenance mode failure, sending 401 response\n")
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprintln(w, "emulated response: 401 FORBIDDEN")
		return
	}

	_, err := os.Stat(a1upgrade.MaintDotFlag)
	if os.IsNotExist(err) {
		thPrint("did not find maint mode flag, sending 401 response\n")
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprintln(w, "emulated response: 401 FORBIDDEN")
		return
	}

	thPrint("found maint mode flag, sending 503 response\n")
	w.WriteHeader(http.StatusServiceUnavailable)
	fmt.Fprintln(w, "emulated response: 503 SERVICE UNAVAILABLE")
}

func handleChefServerReq(w http.ResponseWriter, r *http.Request) {
	thPrintf("stub API received request: %v\n", r)

	if regexp.MustCompile(`/organizations/\w+/required_recipe`).MatchString(r.URL.Path) {
		if a1upgrade.Failure(a1upgrade.MaintModeRequiredRecipeRetryTest) && rrRetryTestCount <= 4 {
			thPrintf("simulating maintenance mode temporary failure %d/5\n", rrRetryTestCount)
			rrRetryTestCount++
			w.WriteHeader(http.StatusForbidden)
			fmt.Fprintln(w, "emulated response: 401 FORBIDDEN")
			return
		}

		if a1upgrade.Failure(a1upgrade.MaintModeRequiredRecipeNotDown) {
			thPrint("simulating maintenance mode failure, sending 401 response\n")
			w.WriteHeader(http.StatusForbidden)
			fmt.Fprintln(w, "emulated response: 401 FORBIDDEN")
			return
		}
	}

	_, err := os.Stat(ChefServerNginxStoppedSentinel)
	if os.IsNotExist(err) {
		thPrint("did not find maint mode flag, sending 401 response\n")
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprintln(w, "emulated response: 401 FORBIDDEN")
		return
	}

	thPrint("found stopped sentinel, sending 502 response\n")
	w.WriteHeader(http.StatusBadGateway)
	fmt.Fprintln(w, "emulated response: 502 BAD GATEWAY")
}

func handleStatus(w http.ResponseWriter, r *http.Request) {
	thPrintf("stub API received request: %v\n", r)
	if a1upgrade.Failure(a1upgrade.PreflightA1Unhealthy) {
		thPrintf("simulating A1 api/_status failure response\n")
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprintln(w, "emulated response: 500 Internal Service Error")
		return
	}

	w.WriteHeader(http.StatusOK)
	fmt.Fprintln(w, "emulated response: 200 OK")
}

var rabbitRetryTestCount = 0

func handleRabbitStats(w http.ResponseWriter, r *http.Request) {
	thPrintf("rabbit api request: %v\n", r)
	// we have to eschew the default routing to prevent go from redirecting
	// requests that decode to having consecutive slashes
	//("/%2f" => "//" => redirect) so we have to do our own "routing" here:
	if r.RequestURI != "/api/queues/%2Finsights/data-collector" {
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprintf(w, "No action defined for %s\n", r.RequestURI)
		return
	}
	user, pass, authUsed := r.BasicAuth()
	if !authUsed {
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprint(w, "Basic auth must be set on requests to rabbit mgmt API\n")
		return
	}
	thPrintf("rabbit API: got basic auth:%s:%s\n", user, pass)

	stats := a1upgrade.RabbitStats{Messages: 0}
	w.WriteHeader(http.StatusOK)

	switch {
	case a1upgrade.Failure(a1upgrade.MaintModeQueuedDataRetryTest) && rabbitRetryTestCount <= 10:
		thPrintf("Sending rabbit stats for temporary non-empty queue (%d/10)\n", rabbitRetryTestCount)
		rabbitRetryTestCount++
		stats.Messages = 100
	case a1upgrade.Failure(a1upgrade.MaintModeQueuedDataNotProcessed):
		thPrint("Sending rabbit stats for non-empty queue\n")
		stats.Messages = 100
	default:
		thPrint("Sending rabbit stats for empty queue\n")
	}

	data, _ := json.Marshal(stats)
	fmt.Fprintf(w, "%s", data)
}

const totalShards = 420

var finishedShards = 0

func handleEsSnapshotStatus(w http.ResponseWriter, r *http.Request) {
	// printing to the console will bork the progress bar
	//thPrint("Sending es snapshot stats")

	snapStats := a1upgrade.EsSnapshotStats{
		State: "STARTED",
		ShardsStats: a1upgrade.EsShardsStats{
			Done:  finishedShards,
			Total: totalShards,
		},
	}

	if finishedShards >= totalShards {
		snapStats.State = "SUCCESS"
	}

	finishedShards = finishedShards + 10

	aggStats := a1upgrade.EsAggSnapshotStats{
		Snapshots: []a1upgrade.EsSnapshotStats{snapStats},
	}
	data, _ := json.MarshalIndent(aggStats, "", "    ")

	// printing to the console will bork the progress bar
	// thPrintf("sending stats data:\n%s\n", data)
	fmt.Fprintf(w, "%s", data)
}

func handleEsIndicesSettings(w http.ResponseWriter, r *http.Request) {
	thPrintf("sending indicesSettingsJSON\n")
	fmt.Fprint(w, indicesSettingsJSON())
}

func handleEsTaskStatus(w http.ResponseWriter, r *http.Request) {
	thPrintf("sending task complete\n")
	fmt.Fprint(w, `{"completed": true}`)
}

func handleEsIndexCreate(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPut {
		msg := fmt.Sprintf("unsupported method %s for request handler handleEsIndexCreate", r.Method)
		panic(msg)
	}

	fmt.Fprint(w, `{"acknowledged": true}`)
}

func handleEsIndexDelete(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodDelete {
		msg := fmt.Sprintf("unsupported method %s for request handler handleEsIndexCreate", r.Method)
		panic(msg)
	}
	fmt.Fprint(w, `{"acknowledged": true}`)
}

type reindexRequest struct {
	Dest   indexName `json:"dest"`
	Source indexName `json:"source"`
}

type indexName struct {
	Index string `json:"index"`
}

var memoizedPairs map[string]string

func allowedReindexPairs() map[string]string {
	if memoizedPairs != nil {
		return memoizedPairs
	}
	memoizedPairs = make(map[string]string)
	for _, idxOldName := range insightsIndicesNames() {
		memoizedPairs[idxOldName] = fmt.Sprintf("%s-1", idxOldName)
	}
	for _, idxOldName := range complianceIndicesNames() {
		memoizedPairs[idxOldName] = fmt.Sprintf("%s-1", idxOldName)
	}
	return memoizedPairs
}

func handleEsReindex(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		msg := fmt.Sprintf("unsupported method %s for request handler handleEsReindex", r.Method)
		panic(msg)
	}
	data, _ := ioutil.ReadAll(r.Body)
	var reqInfo reindexRequest
	err := json.Unmarshal(data, &reqInfo)
	if err != nil {
		thPrintf("failed parsing request data: %s\n\n%s\n\n", err.Error(), data)
		panic(err.Error())
	}
	allowedPairs := allowedReindexPairs()
	val, ok := allowedPairs[reqInfo.Source.Index]
	if !ok || val != reqInfo.Dest.Index {
		thPrintf("unexpected reindex request. allowed source->dest pairs:\n%v\nrequest data:\n%v\nraw request:\n%s\n", allowedPairs, reqInfo, data)
		panic("illegal reindex request")
	}
	time.Sleep(10 * time.Millisecond)
	fmt.Fprint(w, reindexResponseJSON)
}

// {"actions":[{"add":{"alias":"compliance-2018.04.11","index":"compliance-1-2018.04.11"}}]}

type aliasReq struct {
	Actions []aliasAction `json:"actions"`
}

type aliasAction struct {
	Add aliasSpec `json:"add"`
}

type aliasSpec struct {
	AliasName string `json:"alias"`
	IndexName string `json:"index"`
}

var memoizedAliasPairs map[string]string

func allowedAliasPairs() map[string]string {
	if memoizedAliasPairs != nil {
		return memoizedAliasPairs
	}
	memoizedAliasPairs = make(map[string]string)
	for _, idxOldName := range complianceIndicesNames() {
		memoizedAliasPairs[fmt.Sprintf("%s-1", idxOldName)] = idxOldName
	}
	return memoizedAliasPairs
}
func handleEsAlias(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		msg := fmt.Sprintf("unsupported method %s for request handler handleEsAlias", r.Method)
		panic(msg)
	}
	data, _ := ioutil.ReadAll(r.Body)
	var reqInfo aliasReq
	err := json.Unmarshal(data, &reqInfo)
	if err != nil {
		thPrintf("failed parsing request data: %s\n\n%s\n\n", err.Error(), data)
		panic(err.Error())
	}

	actions := reqInfo.Actions
	if len(actions) != 1 {
		thPrintf("unexpected alias request. expected (1) add action. got data:\n%s\n", data)
		panic("unexpected alias request")
	}
	aliasAction := actions[0].Add

	allowedPairs := allowedAliasPairs()
	val, ok := allowedPairs[aliasAction.IndexName]
	if !ok || val != aliasAction.AliasName {
		thPrintf("unexpected alias request. allowed index->alias pairs:\n%v\nrequest data:\n%v\nraw request:\n%s\n", allowedPairs, reqInfo, data)
		panic("illegal alias request")
	}
	fmt.Fprint(w, `{"acknowledged": true}`)
}

func handleEsIndicesStatsStore(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, statsStoreResponseJSON)
}

func thCtlStop(w http.ResponseWriter, r *http.Request) {
	thPrintf("stub API received request: %v\n", r)
	w.WriteHeader(http.StatusOK)
	fmt.Fprintln(w, "test harness control: STOP")
	go func() {
		// shutdown blocks until all connections are closed so we have to do it
		// async if we want to return a response. This is a bit janky but should be
		// okay for our non-production case
		err := a1APIServer.Shutdown(context.Background())
		if err != nil {
			thPrintf("Failed to stop stub A1 API server: %s", err.Error())
		}
	}()
	go func() {
		err := rabbitAPIServer.Shutdown(context.Background())
		if err != nil {
			thPrintf("Failed to stop stub RabbitMQ server: %s", err.Error())
		}
	}()
	go func() {
		err := esAPIServer.Shutdown(context.Background())
		if err != nil {
			thPrintf("Failed to stop stub ElasticSearch server: %s", err.Error())
		}

	}()
	shutdownPostgreSQLStub()
}

func thCtlCreateBackup(w http.ResponseWriter, r *http.Request) {
	thPrintf("stub API received request: %v\n", r)
	w.WriteHeader(http.StatusOK)
	fmt.Fprintln(w, "test harness control: CREATE-BACKUP")
}

func expectedEsStatsStoreURIPaths() []string {
	paths := []string{}

	// mux.HandleFunc("/elasticsearch/insights-2018.04.11,compliance-2018.04.11/_stats/store", handleEsIndicesStatsStore)
	for _, names := range partitionedComplianceAndInsightsNames() {
		path := fmt.Sprintf("/elasticsearch/%s/_stats/store", strings.Join(names, ","))
		paths = append(paths, path)
	}
	return paths
}

func expectedEsIndicesSettingsURIPaths() []string {
	paths := []string{}

	// mux.HandleFunc("/elasticsearch/.automate,.locky,compliance-2018.04.11,compliance-latest-1,compliance-profiles-2,insights-2018.04.11,node-state-2,saved-searches/_settings", handleEsIndicesSettings)
	for _, names := range partitionedIndicesNames() {
		path := fmt.Sprintf("/elasticsearch/%s/_settings", strings.Join(names, ","))
		paths = append(paths, path)
	}
	return paths
}

func partitionedComplianceAndInsightsNames() [][]string {
	allNames := []string{}
	allNames = append(allNames, complianceIndicesNames()...)
	allNames = append(allNames, insightsIndicesNames()...)
	sort.Strings(allNames)
	return partitionIndexNames(allNames)
}

func partitionedIndicesNames() [][]string {
	allNames := []string{".automate", "saved-searches", ".locky", "node-state-2", "compliance-latest-1", "compliance-profiles-2"}
	allNames = append(allNames, complianceIndicesNames()...)
	allNames = append(allNames, insightsIndicesNames()...)
	sort.Strings(allNames)
	return partitionIndexNames(allNames)
}

func partitionIndexNames(allNames []string) [][]string {
	partitionedNames := [][]string{}
	chunkSize := reindexEs2ChunkSize
	chunkCount := len(allNames) / chunkSize

	for i := 0; i <= chunkCount; i++ {

		lowerBound := i * chunkSize
		upperBound := (i + 1) * chunkSize
		if upperBound > len(allNames) {
			upperBound = len(allNames)
		}

		partitionedNames = append(partitionedNames, allNames[lowerBound:upperBound])
	}

	return partitionedNames
}

func complianceIndicesNames() []string {
	list := []string{}
	for i := 0; i < daysToGenerate; i++ {
		list = append(list, fmt.Sprintf("compliance-2018.04.%d", i))
	}
	return list
}

func insightsIndicesNames() []string {
	list := []string{}
	for i := 0; i < daysToGenerate; i++ {
		list = append(list, fmt.Sprintf("insights-2018.04.%d", i))
	}
	return list
}

func indicesSettingsJSON() string {
	var json strings.Builder
	for _, idxName := range complianceIndicesNames() {
		fmt.Fprintf(&json, complianceIndexTemplate, idxName)
	}
	for _, idxName := range insightsIndicesNames() {
		fmt.Fprintf(&json, insightsIndexTemplate, idxName)
	}
	return fmt.Sprintf(indicesSettingsJSONTemplate, (&json).String())
}

const complianceIndexTemplate = `
  "%s": {
    "settings": {
      "index": {
        "refresh_interval": "5s",
        "number_of_shards": "5",
        "creation_date": "1523405301858",
        "analysis": {
          "filter": {
            "autocomplete_filter": {
              "type": "edge_ngram",
              "min_gram": "2",
              "max_gram": "15"
            }
          },
          "analyzer": {
            "autocomplete": {
              "filter": [
                "lowercase",
                "autocomplete_filter"
              ],
              "type": "custom",
              "tokenizer": "standard"
            }
          }
        },
        "number_of_replicas": "1",
        "uuid": "yUQ9AUHcRCKyjYNEerVc5A",
        "version": {
          "created": "2040199",
          "upgraded": "5060999"
        }
      }
    }
  },
`

const insightsIndexTemplate = `
  "%s": {
    "settings": {
      "index": {
        "creation_date": "1523404805478",
        "refresh_interval": "5s",
        "number_of_shards": "5",
        "number_of_replicas": "1",
        "uuid": "uDN2kNvzR0OksV6iz70pdA",
        "version": {
          "created": "2040199",
          "upgraded": "5060999"
        }
      }
    }
  },
`

const indicesSettingsJSONTemplate = `
{
  "node-state-2": {
    "settings": {
      "index": {
        "refresh_interval": "5s",
        "number_of_shards": "5",
        "provided_name": "node-state-2",
        "creation_date": "1525215897785",
        "number_of_replicas": "1",
        "uuid": "7400tUiiRACEBye2VCo72g",
        "version": {
          "created": "5040199",
          "upgraded": "5060999"
        }
      }
    }
  },
	%s
  "compliance-profiles-2": {
    "settings": {
      "index": {
        "refresh_interval": "5s",
        "number_of_shards": "5",
        "provided_name": "compliance-profiles-2",
        "creation_date": "1525216198471",
        "analysis": {
          "analyzer": {
            "autocomplete": {
              "filter": [
                "lowercase"
              ],
              "tokenizer": "autocomplete_tokenizer"
            }
          },
          "tokenizer": {
            "autocomplete_tokenizer": {
              "token_chars": [
                "letter",
                "digit"
              ],
              "min_gram": "2",
              "type": "edge_ngram",
              "max_gram": "20"
            }
          }
        },
        "number_of_replicas": "1",
        "uuid": "SPMpP_JmQ4OwM-Guhmxtrw",
        "version": {
          "created": "5040199",
          "upgraded": "5060999"
        }
      }
    }
  },
  ".automate": {
    "settings": {
      "archived": {
        "index": {
          "create": ".automate index"
        }
      },
      "index": {
        "creation_date": "1501695119090",
        "number_of_shards": "5",
        "number_of_replicas": "1",
        "uuid": "ydE98phRRx6rACBW4SSn3w",
        "version": {
          "created": "2040199",
          "upgraded": "5060999"
        }
      }
    }
  },
  "saved-searches": {
    "settings": {
      "index": {
        "creation_date": "1501708108511",
        "number_of_shards": "5",
        "number_of_replicas": "1",
        "uuid": "sjnSdqw8QsWklTL3EhFftA",
        "version": {
          "created": "2040199",
          "upgraded": "5060999"
        }
      }
    }
  },
  ".locky": {
    "settings": {
      "index": {
        "creation_date": "1525215897006",
        "number_of_shards": "5",
        "number_of_replicas": "1",
        "uuid": "oOekuTpARZK4Ef2HQ2PRjA",
        "version": {
          "created": "5040199",
          "upgraded": "5060999"
        },
        "provided_name": ".locky"
      }
    }
  },
  "compliance-latest-1": {
    "settings": {
      "index": {
        "refresh_interval": "5s",
        "number_of_shards": "5",
        "provided_name": "compliance-latest-1",
        "creation_date": "1525216213499",
        "analysis": {
          "analyzer": {
            "autocomplete": {
              "filter": [
                "lowercase"
              ],
              "tokenizer": "autocomplete_tokenizer"
            },
            "lowercase_keyword": {
              "filter": "lowercase",
              "tokenizer": "keyword"
            }
          },
          "tokenizer": {
            "autocomplete_tokenizer": {
              "token_chars": [
                "letter",
                "digit"
              ],
              "min_gram": "2",
              "type": "edge_ngram",
              "max_gram": "20"
            }
          }
        },
        "number_of_replicas": "1",
        "uuid": "PqSjMU-yTP6iiQRSzdGo6A",
        "version": {
          "created": "5040199",
          "upgraded": "5060999"
        }
      }
    }
  }
}
`

const reindexResponseJSON = `
{
  "took": 1000,
  "timed_out": false,
  "total": 5,
  "updated": 0,
  "created": 5,
  "deleted": 0,
  "batches": 1,
  "noops": 0,
  "version_conflicts": 0,
  "retries": {
    "bulk": 0,
    "search": 0
  },
  "throttled_millis": 0,
  "requests_per_second": 1,
  "throttled_until_millis": 0,
  "failures": [ ]
}
`

const statsStoreResponseJSON = `
{
  "_shards": {
    "total": 20,
    "successful": 10,
    "failed": 0
  },
  "_all": {
    "primaries": {
      "store": {
        "size_in_bytes": 1341941516,
        "throttle_time_in_millis": 0
      }
    },
    "total": {
      "store": {
        "size_in_bytes": 1341941516,
        "throttle_time_in_millis": 0
      }
    }
  },
  "indices": {
    "compliance-2018.04.11": {
      "primaries": {
        "store": {
          "size_in_bytes": 4643901,
          "throttle_time_in_millis": 0
        }
      },
      "total": {
        "store": {
          "size_in_bytes": 4643901,
          "throttle_time_in_millis": 0
        }
      }
    },
    "insights-2018.04.11": {
      "primaries": {
        "store": {
          "size_in_bytes": 1337297615,
          "throttle_time_in_millis": 0
        }
      },
      "total": {
        "store": {
          "size_in_bytes": 1337297615,
          "throttle_time_in_millis": 0
        }
      }
    }
  }
}
`
