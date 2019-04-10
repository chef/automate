package persistence

import (
	"testing"

	"flag"
	"os"

	//"time"

	//"reflect"

	//"context"

	/*	"github.com/chef/automate/components/compliance-service/api/automate-event/server"
		"github.com/chef/automate/components/compliance-service/feed/persistence/elastic"
		"github.com/chef/automate/components/compliance-service/feed/util"
		"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"*/
	olivere "github.com/olivere/elastic"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	//"github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"
	//"github.com/stretchr/testify/assert"
)

const ESUrl = "http://127.0.0.1:9200/"

var DB struct {
	Store    FeedStore
	ESClient *olivere.Client
}

// Does not run between tests
func TestMain(m *testing.M) {
	logrus.Debug("Start TestMain...")

	esr := relaxting.ES2Backend{
		ESUrl: ESUrl,
	}

	DB.Store = NewFeedStore(&esr)
	c, err := esr.ES2Client()

	if err != nil {
		logrus.Fatalf("Can't run tests, error getting Elasticsearch client: %v", err)
	}

	DB.ESClient = c

	flag.Parse()
	exitCode := m.Run()
	logrus.Debug("End TestMain...")

	// exit
	os.Exit(exitCode)
}

// tests for feed consumer network are no longer valid; feature must
// be updated and tests refactored
/*func TestCreateFeedConsumerNetwork(t *testing.T) {
	logrus.Debug("Starting CreateFeedConsumerNetwork test...")

	var Scanner = util.Producer{
		ID:         "urn:chef:compliance:scan-component",
		Name:       "Scanner",
		ObjectType: "system component",
		PTags:      []string{"chef", "system", "compliance"},
	}

	var Fred = util.Producer{
		ID:         "urn:mycompany:user:fred",
		Name:       "Fred",
		ObjectType: "user",
		PTags:      []string{"mycompany", "engineering department", "compliance team"},
	}

	var Violet = util.Producer{
		ID:         "urn:mycompany:user:violet",
		Name:       "Violet",
		ObjectType: "user",
		PTags:      []string{"mycompany", "engineering department", "compliance team"},
	}

	var N1 = util.FeedConsumerNetwork{
		ID:          "urn:mycompany:user:jethro",
		ProducerIDs: []string{Scanner.ID, Fred.ID, Violet.ID},
		Producers:   []util.Producer{Scanner, Fred, Violet},
		Created:     time.Now().UTC().Format(time.RFC3339),
	}

	tests := []struct {
		name string
		in   *util.FeedConsumerNetwork
		out  bool
		err  error
	}{
		{
			name: "create consumer feed network happy path",
			in:   &N1,
			out:  true,
			err:  nil,
		},
	}

	DB.ESClient.Index().Refresh(mappings.IndexNameFeeds)
	time.Sleep(time.Second)

	for _, test := range tests {
		t.Logf("case: %s", test.name)

		got, err := DB.Store.CreateFeedConsumerNetwork(test.in)

		if got != test.out {
			t.Errorf("got %v, want %v for output", got, test.out)
		}
		if err != test.err {
			t.Errorf("got %v, want %v for error", err, test.err)
		}
	}

	DB.ESClient.DeleteIndex(elastic.FeedConsumerNetworkIndexName).Do(context.Background())
}*/

// tests for feed consumer network are no longer valid; feature must
// be updated and tests refactored
/*func TestGetFeedConsumerNetwork(t *testing.T) {
	logrus.Debug("Starting GetFeedConsumerNetwork test...")

	var Scanner = util.Producer{
		ID:         "urn:chef:compliance:scan-component",
		Name:       "Scanner",
		ObjectType: "system component",
		PTags:      []string{"chef", "system", "compliance"},
	}

	var Fred = util.Producer{
		ID:         "urn:mycompany:user:fred",
		Name:       "Fred",
		ObjectType: "user",
		PTags:      []string{"mycompany", "engineering department", "compliance team"},
	}

	var Violet = util.Producer{
		ID:         "urn:mycompany:user:violet",
		Name:       "Violet",
		ObjectType: "user",
		PTags:      []string{"mycompany", "engineering department", "compliance team"},
	}

	var N1 = util.FeedConsumerNetwork{
		ID:          "urn:mycompany:user:jethro",
		ProducerIDs: []string{Scanner.ID, Fred.ID, Violet.ID},
		Producers:   []util.Producer{Scanner, Fred, Violet},
		Created:     time.Now().UTC().Format(time.RFC3339),
	}

	tests := []struct {
		name string
		in   *util.FeedConsumerNetworkQuery
		out  []*util.FeedConsumerNetwork
		err  error
	}{
		{
			name: "get consumer feed network happy path",
			in:   &util.FeedConsumerNetworkQuery{UserID: N1.ID},
			out:  []*util.FeedConsumerNetwork{&N1},
			err:  nil,
		},
	}

	// create network in db
	success, err := DB.Store.CreateFeedConsumerNetwork(&N1)

	if err != nil {
		t.Errorf("Error creating test data: %v", err)
	}

	if success != true {
		t.Error("Create test data operation failed")
	}

	DB.ESClient.Index().Refresh(elastic.FeedConsumerNetworkIndexName)
	time.Sleep(time.Second)

	for _, test := range tests {
		t.Logf("case: %s", test.name)

		got, err := DB.Store.GetFeedConsumerNetwork(test.in)

		if len(got) != len(test.out) {
			t.Errorf("got %d networks, want %d networks as output", len(got), len(test.out))
		}

		for i, n := range got {
			if reflect.DeepEqual(test.out[i], n) != true {
				t.Errorf("got %v, want %v for output", n.ToString()+"\n", test.out[i].ToString()+"\n")
			}
		}

		if err != test.err {
			t.Errorf("got %v, want %v for error", err, test.err)
		}
	}

	DB.ESClient.DeleteIndex(elastic.FeedConsumerNetworkIndexName).Do(context.Background())
} */

/*func TestGetFeedEntries(t *testing.T) {
	logrus.Debug("Starting GetFeedEntries test...")

	var Scanner = util.Producer{
		ID:         "urn:chef:compliance:scan-component",
		Name:       "Scanner",
		ObjectType: "system component",
		PTags:      []string{"chef", "system", "compliance"},
	}

	var Fred = util.Producer{
		ID:         "urn:mycompany:user:fred",
		Name:       "Fred",
		ObjectType: "user",
		PTags:      []string{"mycompany", "engineering department", "compliance team"},
	}

	var Violet = util.Producer{
		ID:         "urn:mycompany:user:violet",
		Name:       "Violet",
		ObjectType: "user",
		PTags:      []string{"mycompany", "engineering department", "compliance team"},
	}

	// Jethro's network. Jethro cares about Scan Jobs and anything initiated by Fred or Violet.
	var N1 = util.FeedConsumerNetwork{
		ID:          "urn:mycompany:user:jethro",
		ProducerIDs: []string{Scanner.ID, Fred.ID, Violet.ID},
		Producers:   []util.Producer{Scanner, Fred, Violet},
		Created:     time.Now().UTC().Format(time.RFC3339),
	}

	var Profiles = util.Producer{
		ID:         "urn:chef:compliance:profile-component",
		Name:       "Profiles",
		ObjectType: "system component",
		PTags:      []string{"chef", "system", "compliance"},
	}

	// Fred's network. Fred cares about Profiles.
	var N2 = util.FeedConsumerNetwork{
		ID:          "urn:mycompany:user:fred",
		ProducerIDs: []string{Profiles.ID},
		Producers:   []util.Producer{Profiles},
		Created:     time.Now().UTC().Format(time.RFC3339),
	}

	var VioletFeedEntry = util.FeedEntry{
		ID:                 uuid.Must(uuid.NewV4()).String(),
		ProducerID:         "urn:mycompany:user:violet",
		ProducerName:       "Violet",
		ProducerObjectType: "user",
		ProducerTags:       []string{"mycompany", "engineering department", "compliance team"},
		FeedType:           "event",
		EventType:          server.ScanJobUpdated,
		Tags:               []string{"mygroup", "compliance", "scan"},
		Published:          time.Now().UTC(),
		ActorID:            "urn:mycompany:user:violet",
		ActorObjectType:    "User",
		ActorName:          "Violet",
		Verb:               "update",
		ObjectID:           "urn:chef:compliance:scan-job",
		ObjectObjectType:   "ScanJob",
		ObjectName:         "Scan Job",
		TargetID:           "urn:mycompany:environment:production",
		TargetObjectType:   "Environment",
		TargetName:         "Production",
		Created:            time.Now().UTC(),
	}

	var ScannerFeedEntry = util.FeedEntry{
		ID:                 uuid.Must(uuid.NewV4()).String(),
		ProducerID:         "urn:chef:compliance:scan-component",
		ProducerName:       "Scanner",
		ProducerObjectType: "system component",
		ProducerTags:       []string{"chef", "system", "compliance"},
		FeedType:           "event",
		EventType:          server.ScanJobCreated,
		Tags:               []string{"mygroup", "compliance", "scan"},
		Published:          time.Now().UTC(),
		ActorID:            "urn:mycompany:user:fred",
		ActorObjectType:    "User",
		ActorName:          "Fred",
		Verb:               "create",
		ObjectID:           "urn:chef:compliance:scan-job",
		ObjectObjectType:   "ScanJob",
		ObjectName:         "Scan Job",
		TargetID:           "urn:mycompany:environment:production",
		TargetObjectType:   "Environment",
		TargetName:         "Production",
		Created:            time.Now().UTC(),
	}

	var ProfilesFeedEntry = util.FeedEntry{
		ID:                 uuid.Must(uuid.NewV4()).String(),
		ProducerID:         "urn:chef:compliance:profile-component",
		ProducerName:       "Profiles",
		ProducerObjectType: "system component",
		ProducerTags:       []string{"chef", "system", "compliance"},
		FeedType:           "event",
		EventType:          server.ProfileCreated,
		Tags:               []string{"mygroup", "compliance", "profiles"},
		Published:          time.Now().UTC(),
		ActorID:            "urn:mycompany:user:magnus",
		ActorObjectType:    "User",
		ActorName:          "Magnus",
		Verb:               "create",
		ObjectID:           "urn:chef:compliance:profile",
		ObjectObjectType:   "Profile",
		ObjectName:         "Profile",
		TargetID:           "urn:mycompany:environment:qa",
		TargetObjectType:   "Environment",
		TargetName:         "QA",
		Created:            time.Now().UTC(),
	}

	tests := []struct {
		name string
		in   *util.FeedQuery
		out  []*util.FeedEntry
		hits int64
		err  error
	}{
		{
			name: "get feed by consumer: happy path",
			in:   &util.FeedQuery{UserID: N1.ID, Size: 10, Filters: nil},
			out:  []*util.FeedEntry{&VioletFeedEntry, &ScannerFeedEntry},
			hits: 2,
			err:  nil,
		},
		{
			name: "get feed by filters: happy path",
			in:   &util.FeedQuery{UserID: N1.ID, Size: 10, Filters: []string{"tags:profiles", "event_type:scanJobCreated"}},
			out:  []*util.FeedEntry{&VioletFeedEntry, &ScannerFeedEntry},
			hits: 2,
			err:  nil,
		},
		{
			name: "get feed all: happy path",
			in:   &util.FeedQuery{UserID: "", Size: 10, Filters: nil},
			out:  []*util.FeedEntry{&VioletFeedEntry, &ScannerFeedEntry, &ProfilesFeedEntry},
			hits: 3,
			err:  nil,
		},
	}

	// create network 1 in db
	success, err := DB.Store.CreateFeedConsumerNetwork(&N1)

	if err != nil {
		t.Errorf("Error creating test data: %v", err)
	}

	if success != true {
		t.Error("Create test data operation failed")
	}

	// create network 2 in db
	success, err = DB.Store.CreateFeedConsumerNetwork(&N2)

	if err != nil {
		t.Errorf("Error creating test data: %v", err)
	}

	if success != true {
		t.Error("Create test data operation failed")
	}

	// make sure networks are visible in db
	DB.ESClient.Index().Refresh(elastic.FeedConsumerNetworkIndexName)
	time.Sleep(time.Second)

	// create feed entries
	success, err = DB.Store.CreateFeedEntry(&VioletFeedEntry)

	if err != nil {
		t.Errorf("Error creating test data: %v", err)
	}

	if success != true {
		t.Error("Create test data operation failed")
	}

	success, err = DB.Store.CreateFeedEntry(&ScannerFeedEntry)

	if err != nil {
		t.Errorf("Error creating test data: %v", err)
	}

	if success != true {
		t.Error("Create test data operation failed")
	}

	success, err = DB.Store.CreateFeedEntry(&ProfilesFeedEntry)

	if err != nil {
		t.Errorf("Error creating test data: %v", err)
	}

	if success != true {
		t.Error("Create test data operation failed")
	}

	DB.ESClient.Index().Refresh(mappings.IndexNameFeeds)
	time.Sleep(time.Second)

	for _, test := range tests {
		t.Logf("case: %s", test.name)

		got, gotHits, err := DB.Store.GetFeed(test.in)

		if len(got) != len(test.out) {
			t.Errorf("got %d feed entries, want %d feed entries as output", len(got), len(test.out))
		}

		assert.ElementsMatch(t, test.out, got)
		assert.Equal(t, test.hits, gotHits)

		if err != test.err {
			t.Errorf("got %v, want %v for error", err, test.err)
		}
	}

	DB.ESClient.DeleteIndex(elastic.FeedConsumerNetworkIndexName).Do(context.Background())
	DB.ESClient.DeleteIndex(mappings.IndexNameFeeds).Do(context.Background())
} */

/*func TestPage(t *testing.T) {
	logrus.Debug("Starting Pagination test...")
	numEntries := 57
	for i := 0; i <= numEntries; i++ {
		success, err := DB.Store.CreateFeedEntry(&TestData.VioletEvent)

		if err != nil {
			t.Errorf("Error creating test data: %v", err)
		}

		if success != true {
			t.Error("Create test data operation failed")
		}
	}

	DB.ESClient.Index().Refresh(elastic.FeedsIndexName)
	time.Sleep(time.Second)

	/*tests := []struct {
		name string
		in   *util.FeedQuery
		out  []util.FeedEntry
		err  error
	}{
		{
			name: "test pagination: segment 1",
			in:   &util.FeedQuery{UserID: TestData.N1.ID},
			out:  []util.FeedEntry{TestData.VioletEvent, TestData.ScannerEvent},
			err:  nil,
		},
		{
			name: "test pagination: segment 2",
			in:   &util.FeedQuery{UserID: ""},
			out:  []util.FeedEntry{TestData.VioletEvent, TestData.ScannerEvent, TestData.ProfilesEvent},
			err:  nil,
		},
	}
}*/

func TestSort(t *testing.T) {}
