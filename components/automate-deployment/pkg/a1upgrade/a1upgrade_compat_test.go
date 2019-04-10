package a1upgrade

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Workflow

func TestFIPSIsConfigured(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.FipsConfigured(true)
	require.NoError(t, err)
	output := checker.Msgs.String()
	assert.Equal(t, fipsDetectedMsg, output)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
}

func TestFIPSIsNotConfigured(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.FipsConfigured(false)
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Msgs.Len())
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
}

func TestSAMLIsConfigured(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.SAMLConfigured(true)
	require.NoError(t, err)
	output := checker.Msgs.String()
	assert.Equal(t, samlDetectedMsg, output)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 1, checker.Warnings)
}

func TestSAMLIsNotConfigured(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.SAMLConfigured(false)
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Msgs.Len())
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
}

func TestMultipleElasticsearchURLS(t *testing.T) {
	es_urls := []string{"https://es.node1.com", "https://es.node2.com", "https://es.node3.com"}
	checker := NewCompatChecker()
	err := checker.ExternalElasticsearchConfigured(es_urls)
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, externalESEnabledMsg, output)
}

func TestNonLocalElasticsearchURLS(t *testing.T) {
	es_urls := []string{"https://es.node1.com"}
	checker := NewCompatChecker()
	err := checker.ExternalElasticsearchConfigured(es_urls)
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, externalESMayBeEnabledMsg, output)
}

func TestLocalElasticsearchURLS(t *testing.T) {
	local_urls := []string{"https://localhost", "https://127.0.0.1"}
	for _, u := range local_urls {
		checker := NewCompatChecker()
		es_url := []string{u}
		err := checker.ExternalElasticsearchConfigured(es_url)
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
		assert.Equal(t, 0, checker.Warnings)
		assert.Equal(t, 0, checker.Msgs.Len())
	}
}

func TestProxyIsConfigured(t *testing.T) {
	checker := NewCompatChecker()
	checker.ProxyConfigured("https://proxy.example.com")
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 1, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, proxyEnabledMsg, output)

}

func TestProxyIsNotConfigured(t *testing.T) {
	checker := NewCompatChecker()
	checker.ProxyConfigured("")
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

func TestLocalBackupDirectoryDoesNotExist(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.BackupConfigured("fs", false, "/fake/backup/directory", "")
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, noBackupConfiguredMsg, output)
}

func TestLocalBackupDirectoryExistsAndIsEmpty(t *testing.T) {
	testDir, _ := ioutil.TempDir("", "TestEmptyBackupDir")
	defer os.Remove(testDir)

	checker := NewCompatChecker()
	err := checker.BackupConfigured("fs", false, testDir, "")
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, noBackupConfiguredMsg, output)
}

func TestLocalBackupDirNonEmpty(t *testing.T) {
	testDir, _ := ioutil.TempDir("", "TestBackupDir")
	testFile, _ := ioutil.TempFile(testDir, "TestFile")
	defer os.Remove(testFile.Name())
	defer os.Remove(testDir)

	checker := NewCompatChecker()
	err := checker.BackupConfigured("fs", false, testDir, "")
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

func TestS3BackupConfigured(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.BackupConfigured("s3", false, "default-local-backup-dir", "s3-backups-go-here")
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

func TestBackupRetentionConfigured(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.BackupConfigured("fs", true, "default-local-backup-dir", "")
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

func TestPrimaryEnabled(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.DisasterRecoveryConfigured("primary.com", "")
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, disasterRecoveryEnabledMsg, output)
}

func TestSecondaryEnabled(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.DisasterRecoveryConfigured("", "secondary.com")
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, disasterRecoveryEnabledMsg, output)
}

func TestDisasterRecoveryOff(t *testing.T) {
	checker := NewCompatChecker()
	err := checker.DisasterRecoveryConfigured("", "")
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

func TestWorkflowGitReposDirDoesNotExist(t *testing.T) {
	// Use a fake directory so that the check will fail
	gitReposDir := "/path/that/does/not/exist"

	checker := NewCompatChecker()
	err := checker.WorkflowGitReposValid(gitReposDir)
	require.NoError(t, err)
	assert.Equal(t, 1, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	output := checker.Msgs.String()
	assert.Equal(t, workflowDirMissingMsg, output)

}

func TestWorkflowGitReposDirExistWithData(t *testing.T) {
	gitReposDir, _ := ioutil.TempDir("", "TestGitReposDir")
	// Create a subdirectory of the workflow directory so that the workflow
	// directory will be non-empty. We don't use this subdirectory otherwise.
	_, _ = ioutil.TempDir(gitReposDir, "WorkflowSubdir")
	defer os.RemoveAll(gitReposDir)

	checker := NewCompatChecker()
	err := checker.WorkflowGitReposValid(gitReposDir)
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

// @afiune Do we care if the git_repos dir is empty?
func TestWorkflowGitReposDirExistWithNOData(t *testing.T) {
	gitReposDir, _ := ioutil.TempDir("", "TestGitReposDir")
	defer os.Remove(gitReposDir)

	checker := NewCompatChecker()
	err := checker.WorkflowGitReposValid(gitReposDir)
	require.NoError(t, err)
	assert.Equal(t, 0, checker.Failures)
	assert.Equal(t, 0, checker.Warnings)
	assert.Equal(t, 0, checker.Msgs.Len())
}

func TestRunningMarketplaceImage(t *testing.T) {
	t.Run("the config is invalid when chef-marketplace is installed", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		testDir := path.Join(testDirBase, "chef-marketplace")
		_ = os.Mkdir(testDir, 0755)

		defer os.RemoveAll(testDirBase)

		checker := NewCompatChecker()
		err := checker.RunningMarketplaceImage(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is valid when chef-marketplace is not installed", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		// some other omnibus package that is not explicitly banned
		testDir := path.Join(testDirBase, "chef-client")
		_ = os.Mkdir(testDir, 0755)

		defer os.Remove(testDir)

		checker := NewCompatChecker()
		err := checker.RunningMarketplaceImage(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
	})
}

func TestCSElasticConfigValid(t *testing.T) {

	a1EsConfig := NewA1Config().DeliveryRunning.Delivery.Elasticsearch
	a1EsConfig.NginxProxyURL = "http://localhost:8080/elasticsearch/"

	erchefRawJSON := []byte(`{ "private_chef": { "opscode-erchef": { "search_provider": "elasticsearch", "search_queue_mode": "batch" } } }`)
	var validErchefCSR ChefServerRunning
	err := json.Unmarshal(erchefRawJSON, &validErchefCSR)
	require.NoError(t, err)
	validErchefConfig := validErchefCSR.PrivateChef.OpscodeErchef

	t.Run("the config is invalid when opscode-solr4 isn't enabled", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "opscode-solr4": { "enable": false, "external": true, "external_url": "http://localhost:8080/elasticsearch" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerElasticsearchConfigValid(&csr.PrivateChef.OpscodeSolr4, &a1EsConfig, &validErchefConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when opscode-solr4 isn't external", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "opscode-solr4": { "enable": true, "external": false, "external_url": "http://localhost:8080/elasticsearch" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerElasticsearchConfigValid(&csr.PrivateChef.OpscodeSolr4, &a1EsConfig, &validErchefConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when the opscode-solr4 external URL doesn't match a1's elasticsearch", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "opscode-solr4": { "enable": true, "external": true, "external_url": "http://otherhost.example:8080/elasticsearch" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerElasticsearchConfigValid(&csr.PrivateChef.OpscodeSolr4, &a1EsConfig, &validErchefConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when the opscode_erchef is not configured for search_provider elasticsearch", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "opscode-solr4": { "enable": true, "external": true, "external_url": "http://localhost:8080/elasticsearch" } } }`)

		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)

		erchefRawJSON := []byte(`{ "private_chef": { "opscode-erchef": { "search_provider": "solr", "search_queue_mode": "batch" } } }`)
		var invalidErchefCSR ChefServerRunning
		err = json.Unmarshal(erchefRawJSON, &invalidErchefCSR)
		require.NoError(t, err)

		invalidErchefConfig := invalidErchefCSR.PrivateChef.OpscodeErchef

		err = checker.ChefServerElasticsearchConfigValid(&csr.PrivateChef.OpscodeSolr4, &a1EsConfig, &invalidErchefConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when the opscode_erchef is not configured for search_queue_mode batch", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "opscode-solr4": { "enable": true, "external": true, "external_url": "http://localhost:8080/elasticsearch" } } }`)

		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)

		erchefRawJSON := []byte(`{ "private_chef": { "opscode-erchef": { "search_provider": "elasticsearch", "search_queue_mode": "rabbitmq" } } }`)
		var invalidErchefCSR ChefServerRunning
		err = json.Unmarshal(erchefRawJSON, &invalidErchefCSR)
		require.NoError(t, err)

		invalidErchefConfig := invalidErchefCSR.PrivateChef.OpscodeErchef

		err = checker.ChefServerElasticsearchConfigValid(&csr.PrivateChef.OpscodeSolr4, &a1EsConfig, &invalidErchefConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is valid when opscode-solr4 is enabled, external, and the URL matches a1", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "opscode-solr4": { "enable": true, "external": true, "external_url": "http://localhost:8080/elasticsearch" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerElasticsearchConfigValid(&csr.PrivateChef.OpscodeSolr4, &a1EsConfig, &validErchefConfig)
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
	})
}

func TestCSPostgresConfigValid(t *testing.T) {

	a1PgConfig := NewA1Config().DeliveryRunning.Delivery.PostgreSQL
	a1PgConfig.Vip = "127.0.0.1"
	a1PgConfig.Port = json.Number("5432")

	t.Run("the config is invalid when postgresql isn't enabled", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "postgresql": { "enable": false, "external": true, "vip": "127.0.0.1", "port": 5432 } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerPostgresConfigValid(&csr.PrivateChef.Postgresql, &a1PgConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when postgresql isn't external", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "postgresql": { "enable": true, "external": false, "vip": "127.0.0.1", "port": 5432 } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerPostgresConfigValid(&csr.PrivateChef.Postgresql, &a1PgConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when the postgresql vip doesn't match a1", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "postgresql": { "enable": true, "external": true, "vip": "192.168.255.254", "port": 5432 } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerPostgresConfigValid(&csr.PrivateChef.Postgresql, &a1PgConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when the postgresql port doesn't match a1", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "postgresql": { "enable": true, "external": true, "vip": "127.0.0.1", "port": 7777 } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerPostgresConfigValid(&csr.PrivateChef.Postgresql, &a1PgConfig)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is valid when pg is enabled, external, and the vip and port match a1's pg", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "postgresql": { "enable": true, "external": true, "vip": "127.0.0.1", "port": 5432 } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerPostgresConfigValid(&csr.PrivateChef.Postgresql, &a1PgConfig)
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
	})
}

func TestCSBookshelfConfigValid(t *testing.T) {
	t.Run("the config is invalid when bookshelf isn't enabled", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "bookshelf": { "enable": false, "storage_type": "sql" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerBookshelfConfigValid(&csr.PrivateChef.Bookshelf)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run(`the config is invalid when bookshelf doesn't have storage_type=="sql"`, func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "bookshelf": { "enable": true, "storage_type": "filesystem" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerBookshelfConfigValid(&csr.PrivateChef.Bookshelf)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run(`the config is valid when bookshelf is enabled and uses sql storage`, func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "bookshelf": { "enable": true, "storage_type": "sql" } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.ChefServerBookshelfConfigValid(&csr.PrivateChef.Bookshelf)
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
	})
}

func TestUnsupportedCSAddOnsNotUsed(t *testing.T) {
	t.Run("the config is invalid when chef-manage is used", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		testDir := path.Join(testDirBase, "chef-manage")
		_ = os.Mkdir(testDir, 0755)

		defer os.RemoveAll(testDirBase)

		checker := NewCompatChecker()
		err := checker.UnsupportedCSAddOnsNotUsed(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when opscode-push-jobs-server is used", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		testDir := path.Join(testDirBase, "opscode-push-jobs-server")
		_ = os.Mkdir(testDir, 0755)

		defer os.Remove(testDir)

		checker := NewCompatChecker()
		err := checker.UnsupportedCSAddOnsNotUsed(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when opscode-reporting is used", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		testDir := path.Join(testDirBase, "opscode-reporting")
		_ = os.Mkdir(testDir, 0755)

		defer os.Remove(testDir)

		checker := NewCompatChecker()
		err := checker.UnsupportedCSAddOnsNotUsed(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is invalid when opscode-analytics is used", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		testDir := path.Join(testDirBase, "opscode-analytics")
		_ = os.Mkdir(testDir, 0755)

		defer os.Remove(testDir)

		checker := NewCompatChecker()
		err := checker.UnsupportedCSAddOnsNotUsed(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})

	t.Run("the config is valid when no unsupported add-ons are used", func(t *testing.T) {
		testDirBase, _ := ioutil.TempDir("", "")
		// some other omnibus package that is not explicitly banned
		testDir := path.Join(testDirBase, "chef-client")
		_ = os.Mkdir(testDir, 0755)

		defer os.Remove(testDir)

		checker := NewCompatChecker()
		err := checker.UnsupportedCSAddOnsNotUsed(path.Dir(testDir))
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
	})
}

func TestOcIDNotUsed(t *testing.T) {
	t.Run("the config is invalid when chef-server-running has oc_id entries", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "oc_id": { "applications": { "supermarket": { "redirect_uri": "https://supermarket.example" } } } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.OcIDNotUsed(&csr.PrivateChef.OcID)
		require.NoError(t, err)
		assert.Equal(t, 1, checker.Failures)
	})
	t.Run("the config is valid when chef-server-running has no oc_id entries", func(t *testing.T) {
		checker := NewCompatChecker()
		rawJson := []byte(`{ "private_chef": { "oc_id": { "applications": {  } } } }`)
		var csr ChefServerRunning
		err := json.Unmarshal(rawJson, &csr)
		require.NoError(t, err)
		err = checker.OcIDNotUsed(&csr.PrivateChef.OcID)
		require.NoError(t, err)
		assert.Equal(t, 0, checker.Failures)
	})
}
