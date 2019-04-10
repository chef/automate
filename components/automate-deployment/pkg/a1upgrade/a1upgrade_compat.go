// Copyright © 2017 Chef Software

package a1upgrade

import (
	"fmt"
	"io"
	"net/url"
	"os"
	"path"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/pg"
)

const fipsDetectedMsg = `
    * FAILURE: Detected unsupported configuration settings in your Chef Automate v1 installation:

          fips['enable']=true - Chef Automate v2 does not yet support FIPS operating mode

      You may ignore this warning by passing the --skip-fips-check flag
      to the upgrade command and running again.
`

const samlDetectedMsg = `
    * WARNING: SAML config in your Chef Automate v1 installation detected:

      Chef Automate v2 cannot migrate existing SAML settings due to incompatibilities
      between v1 and v2 SAML functionality.

      You may stop this upgrade and run:

          chef-automate upgrade-from-v1 gen-config --skip-saml-check

      This upgrade command will generate a configuration for your Chef Automate v2
      installation and offer you the opportunity to edit it. The SAML section of
      "Configuring Chef Automate" explains how to configure SAML for Chef Automate v2.

          https://automate.chef.io/docs/configuration/#saml

      Alternatively, you may ignore this warning and continue this upgrade by entering 'y' below.
      You may set up SAML for Chef Automate v2 anytime after upgrading.
`

const noBackupConfiguredMsg = `
    * FAILURE: No backup settings are configured for your Chef Automate v1 installation.

      We recommend that you backup all production installations of Chef Automate v1
      before upgrading to v2. The upgrade process will apply irreversible
      data transformations, so a backup will be the only way to revert to Chef
      Automate v1 in some cases.

      If this is a non-production system or if you use a different method of backing
      up your Chef Automate installation, you may add the --skip-backup-check flag to the
      upgrade command and try again.
`

const externalESEnabledMsg = `
    * FAILURE: Your Chef Automate v1 installation is configured to use an external Elasticsearch
      cluster.

      Automated upgrade for this configuration is in development. To upgrade an external
      Elasticsearch cluster, please reach out to a Customer Success or Customer Support
      representative for assistance.
`

const externalESMayBeEnabledMsg = `
    * FAILURE: Your Chef Automate v1 installation may be configured to use an external Elasticsearch
      cluster.

      Automated upgrade for this configuration is in development. To upgrade an external
      Elasticsearch cluster, please reach out to a Customer Success or Customer Support
      representative for assistance.

      If you are **NOT** using an external Elasticsearch cluster, you may restart your
      upgrade with the --skip-external-es-check flag to proceed.
`

const proxyEnabledMsg = `
    * WARNING: Your Chef Automate v1 configuration includes proxy settings (delivery[proxy]) in your v1 config).
      Later in the upgrade process, we will generate a configuration for your Chef Automate v2
      installation and offer you the opportunity to edit it. You will need to add the following
      settings to your configuration and follow the directions to continue:
         [global.v1.proxy]
         host = "<your proxy host>"
         port = <your proxy port>
         # Uncomment the following lines and set the parameters as necessary.
         # user = "<your proxy user>"
         # password = "<your proxy password>"
         # no_proxy = ["whitelist", "should", "include", "packages.chef.io","raw.githubusercontent.com",
                       "api.bintray.com","bldr.habitat.sh","akamai.bintray.com","dl.bintray.com","bintray.com"]
`

const disasterRecoveryEnabledMsg = `
    * FAILURE: Your Chef Automate v1 configuration sets delivery['primary_ip'] and/or
      delivery['standby_ip'] for disaster recovery.

      Chef Automate v2 does not support disaster recovery. If you still wish to upgrade to Chef Automate v2,
      restart your upgrade with the --skip-disaster-recovery-check flag.
`

const workflowInUseMsg = `
    * FAILURE: The Workflow directory in your Chef Automate v1 installation
      (delivery['git_repos'] in your v1 config) is not empty. Chef Automate v2 does not support
      Workflow. If you still wish to upgrade to Chef Automate v2, restart your upgrade with the
      --skip-workflow-check flag.
`

const workflowDirMissingMsg = `
    * FAILURE: The Workflow directory in your Chef Automate v1 installation
      (delivery['git_repos'] in your v1 config) does not exist.

      Please create or configure an existing directory and try again.
`

const marketplaceImageMsg = `
    * FAILURE: This system appears to be a cloud marketplace image.
      Chef Automate v2 does not support cloud marketplace images.
`

const invalidChefServerPostgresConfigMsg = `
    * FAILURE: You have enabled Chef Server upgrade, but your Chef Server
      configuration is not supported for the integrated upgrade. The integrated
      upgrade only supports the all-in-one topology but your Chef Server
      Postgresql configuration does not match this topology. The integrated
      upgrade requires the following postres configuration:
      * Postgresql is not disabled
      * Chef Server is configured for external Postgresql
      * Chef Server shares Chef Automate v1's Postgresql (identical vip and port)

      If you wish to migrate to the Chef Automate v2 integrated Chef Server
      configuration, first run the upgrade from v1 without Chef Server, then
      enable Chef Server in Automate v2, and then migrate your Chef Server data
      separately.
`

const invalidChefServerSearchConfigMsg = `
    * FAILURE: You have enabled Chef Server upgrade, but your Chef Server
      configuration is not supported for the integrated upgrade. The integrated
      upgrade only supports the all-in-one topology but your Chef Server
      opscode-solr4 configuration does not match this topology. The integrated
      upgrade requires the following search configuration:
      * opscode_erchef search_provider is set to "elasticsearch"
      * opscode_erchef search_queue_mode is set to "batch"
      * 'opscode-solr4' is not disabled
      * 'opscode-solr4' is set to external
      * The 'opscode-solr4' external URL matches Chef Automate v1's nginx_proxy_url

      If you wish to migrate to the Chef Automate v2 integrated Chef Server
      configuration, first run the upgrade from v1 without Chef Server, then
      enable Chef Server in Automate v2, and then migrate your Chef Server data
      separately.
`

const invalidChefServerBookshelfConfigMsg = `
    * FAILURE: You have enabled Chef Server upgrade, but your Chef Server
      configuration is not supported for the integrated upgrade. The integrated
      upgrade only supports the all-in-one topology but your Chef Server
      bookshelf configuration does not match this topology. The integrated
      upgrade requires the following search configuration:
      * bookshelf is enabled
      * bookshelf uses storage type "sql"

      If you wish to migrate to the Chef Automate v2 integrated Chef Server
      configuration, first run the upgrade from v1 without Chef Server, then
      enable Chef Server in Automate v2, and then migrate your Chef Server data
      separately.
`

const chefServerAddOnInUseFmt = `
    * FAILURE: You have the Chef Server add-on %s installed.
      The Chef Automate v2 integrated Chef Server does not support this add-on.
      If you are not using the add-on, you may uninstall the add-on and retry
      the upgrade command to continue.
`

const ocIDInUseMsg = `
    * FAILURE: You have enabled chef server upgrade, but your chef server
      has one or more applications configured for oc_id authentication. The
      Chef Automate v2 integrated Chef Server does not include the oc_id
      application. If you wish to continue, remove the oc_id applications from
      your Chef Server configuration and retry the upgrade.
`

var chefServerAddOns = []string{
	"chef-manage",
	"opscode-push-jobs-server",
	"opscode-reporting",
	"opscode-analytics",
}

const OmnibusInstallRoot = "/opt"

type CompatChecker struct {
	Warnings int
	Failures int
	Msgs     strings.Builder
}

func NewCompatChecker() CompatChecker {
	var b strings.Builder
	return CompatChecker{
		Warnings: 0,
		Failures: 0,
		Msgs:     b,
	}
}

type CompatCheckerSkips struct {
	BackupCheck           bool
	DisasterRecoveryCheck bool
	ExternalESCheck       bool
	FIPSCheck             bool
	SAMLCheck             bool
	WorkflowCheck         bool
}

// @afiune delete me when workflow feature is completed, as well as the skip flags
func (s *CompatCheckerSkips) SkipWorkflowCheck() {
	s.WorkflowCheck = true
}

func (c *CompatChecker) RunAutomateChecks(a1Config *A1Config, skip CompatCheckerSkips) error {
	fips := a1Config.DeliveryRunning.Delivery.FIPS.Enabled
	workflow := a1Config.DeliveryRunning.Delivery.Delivery.GitRepos
	esClusterUrls := a1Config.DeliveryRunning.Delivery.Elasticsearch.ClusterURLS
	proxyHost := a1Config.DeliveryRunning.Delivery.Delivery.Proxy.Host
	backupRetention := a1Config.DeliveryRunning.Delivery.Backup.Retention.Enabled
	backupType := a1Config.DeliveryRunning.Delivery.Backup.Type
	backupLocation := a1Config.DeliveryRunning.Delivery.Backup.Location
	backupS3Bucket := a1Config.DeliveryRunning.Delivery.Backup.S3Bucket
	primaryIP := a1Config.DeliveryRunning.Delivery.Delivery.PrimaryIp
	standbyIP := a1Config.DeliveryRunning.Delivery.Delivery.StandbyIp

	if !skip.BackupCheck {
		err := c.BackupConfigured(backupType, backupRetention, backupLocation, backupS3Bucket)
		if err != nil {
			return err
		}
	}

	if !skip.DisasterRecoveryCheck {
		err := c.DisasterRecoveryConfigured(primaryIP, standbyIP)
		if err != nil {
			return err
		}
	}

	if !skip.ExternalESCheck {
		err := c.ExternalElasticsearchConfigured(esClusterUrls)
		if err != nil {
			return err
		}
	}

	if !skip.FIPSCheck {
		err := c.FipsConfigured(fips)
		if err != nil {
			return err
		}
	}

	if !skip.SAMLCheck {
		// connect to database to check for SAML config in A1
		port, err := a1Config.DeliveryRunning.Delivery.PostgreSQL.Port.Int64()
		if err != nil {
			return errors.Wrap(err, "Could not parse PostgreSQL port from Automate 1 configuration\n")
		}

		connInfo := &pg.A1ConnInfo{
			User: a1Config.DeliveryRunning.Delivery.PostgreSQL.SuperuserUsername,
			Pass: a1Config.DeliverySecrets.Postgresql.SuperuserPassword,
			Host: a1Config.DeliveryRunning.Delivery.PostgreSQL.Vip,
			Port: uint64(port),
		}

		saml, err := CheckSAMLConfig(connInfo)
		if err != nil {
			return errors.Wrap(err, "Could not check for existing SAML config. Retry the upgrade or pass --skip-saml-check to the upgrade command to skip this step.")
		}

		err = c.SAMLConfigured(saml)
		if err != nil {
			return err
		}
	}

	c.ProxyConfigured(proxyHost)

	if !skip.WorkflowCheck {
		err := c.WorkflowInUse(workflow)
		if err != nil {
			return err
		}
	}

	// We do not provide a `--skip-*` flag for this check because it's unlikely
	// to provide any value: we do not have marketplace support for A2; in
	// particular we do not have any implementation of the billing/licensing
	// systems used in marketplace images for A2. So any upgrade would just leave
	// the customer broken from a licensing standpoint (and possibly in other
	// ways as well).
	err := c.RunningMarketplaceImage(OmnibusInstallRoot)
	if err != nil {
		return err
	}

	return nil
}

func (c *CompatChecker) RunChefServerChecks(a1Config *A1Config) error {
	deliveryRunning := a1Config.DeliveryRunning.Delivery
	a1EsConfig := &deliveryRunning.Elasticsearch
	a1PgConfig := &deliveryRunning.PostgreSQL

	chefServerConfig := a1Config.ChefServerRunning.PrivateChef
	erchefConfig := &chefServerConfig.OpscodeErchef
	csPostgresConfig := &chefServerConfig.Postgresql
	csSearchConfig := &chefServerConfig.OpscodeSolr4
	bookshelfConfig := &chefServerConfig.Bookshelf
	ocIDConfig := &chefServerConfig.OcID

	err := c.ChefServerElasticsearchConfigValid(csSearchConfig, a1EsConfig, erchefConfig)
	if err != nil {
		return err
	}
	err = c.ChefServerPostgresConfigValid(csPostgresConfig, a1PgConfig)
	if err != nil {
		return err
	}
	err = c.ChefServerBookshelfConfigValid(bookshelfConfig)
	if err != nil {
		return err
	}
	err = c.UnsupportedCSAddOnsNotUsed(OmnibusInstallRoot)
	if err != nil {
		return err
	}
	err = c.OcIDNotUsed(ocIDConfig)
	if err != nil {
		return err
	}

	return nil
}

func (c *CompatChecker) FipsConfigured(fips bool) error {
	if !fips {
		return nil
	}
	_, err := c.Msgs.WriteString(fipsDetectedMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

func (c *CompatChecker) SAMLConfigured(saml bool) error {
	if Failure(ConfigConflictSAML) {
		fmt.Fprintln(os.Stderr, samlDetectedMsg)
		return errors.New("simulated SAML conflict")
	}

	if !saml {
		return nil
	}
	_, err := c.Msgs.WriteString(samlDetectedMsg)
	if err != nil {
		return err
	}
	c.Warnings++
	return nil
}

func (c *CompatChecker) ExternalElasticsearchConfigured(clusterURLS []string) error {
	if len(clusterURLS) > 1 {
		// If clusterURLS contains > 1 elements, elasticsearch is external.
		_, err := c.Msgs.WriteString(externalESEnabledMsg)
		if err != nil {
			return err
		}
		c.Failures++
		return nil
	} else {
		// TODO: verify elasticsearch_urls in the A1 config is always set to something.
		// This assumes it is.
		thing, err := url.Parse(clusterURLS[0])
		if err != nil {
			return err
		}
		hostname := thing.Hostname()
		if hostname == "127.0.0.1" || hostname == "localhost" {
			return nil
		}

		_, err = c.Msgs.WriteString(externalESMayBeEnabledMsg)
		if err != nil {
			return err
		}
		c.Failures++
		return nil
	}
}

func (c *CompatChecker) ProxyConfigured(proxy string) {
	if proxy == "" {
		return
	}
	c.Msgs.WriteString(proxyEnabledMsg)
	c.Warnings++
}

func (c *CompatChecker) BackupConfigured(backupType string, retention bool, backupLocation string, backupS3Bucket string) error {
	// A1 configures local backups by default, so the presence of a backup configuration
	// is not sufficient. We think people are probably doing backups if one of the following is true:
	// 1. They have local backups configured and their local backup directory backupLocation exists and is non-empty.
	// 2. They have s3 backups configured and an backupS3Bucket configured.
	// 3. They have a retention (backup cron) configured.

	// local backup case
	_, err := os.Stat(backupLocation)
	if err != nil {
		// If the error is that the file does not exist, keep going.
		if !os.IsNotExist(err) {
			return err
		}
	} else {
		dircheck, derr := directoryEmpty(backupLocation)
		if derr != nil {
			return derr
		}

		if backupType == "fs" && !dircheck {
			return nil
		}
	}

	// s3 backup case
	if backupType == "s3" && backupS3Bucket != "" {
		return nil
	}

	// retention case
	if retention {
		return nil
	}

	_, err = c.Msgs.WriteString(noBackupConfiguredMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

func (c *CompatChecker) DisasterRecoveryConfigured(primary string, standby string) error {
	if primary == "" && standby == "" {
		return nil
	}

	_, err := c.Msgs.WriteString(disasterRecoveryEnabledMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

// RunWorkflowChecks run all the workflow checks to verify compatibility with A2
//
// TODO: @afiune here we can verify everything wee need to verify,
// correct config, existing git-repos, valid ssh-keys, etc.
func (c *CompatChecker) RunWorkflowChecks(a1Config *A1Config) error {
	workflowGitDir := a1Config.DeliveryRunning.Delivery.Delivery.GitRepos

	err := c.WorkflowGitReposValid(workflowGitDir)
	if err != nil {
		return err
	}

	return nil
}

// WorkflowGitReposValid
func (c *CompatChecker) WorkflowGitReposValid(gitReposDir string) error {
	_, err := os.Stat(gitReposDir)
	if os.IsNotExist(err) {
		_, err = c.Msgs.WriteString(workflowDirMissingMsg)
		if err != nil {
			return err
		}
		c.Failures++
		return nil
	}

	// @afiune Do we care if the git_repos dir is empty?

	return nil
}

// WorkflowInUse
func (c *CompatChecker) WorkflowInUse(workflowDir string) error {
	// The workflow dir `git_repos` is always set in the A1 config.
	// We look to see whether the configured directory contains anything
	// to decide if workflow is in use.
	_, err := os.Stat(workflowDir)
	if os.IsNotExist(err) {
		return nil
	}

	dircheck, err := directoryEmpty(workflowDir)
	if err != nil {
		return err
	}
	if dircheck {
		return nil
	}

	_, err = c.Msgs.WriteString(workflowInUseMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

func (c *CompatChecker) RunningMarketplaceImage(omnibusRoot string) error {
	marketplacePkgPath := path.Join(omnibusRoot, "chef-marketplace")
	_, err := os.Stat(marketplacePkgPath)
	if os.IsNotExist(err) {
		return nil
	}
	_, err = c.Msgs.WriteString(marketplaceImageMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

func (c *CompatChecker) ChefServerElasticsearchConfigValid(csEsConfig *OpscodeSolr4,
	a1EsConfig *DeliveryRunningElasticsearch,
	csErchefConfig *OpscodeErchef) error {

	esEnabled := csEsConfig.Enable
	esExternal := csEsConfig.External

	csURL, err := url.Parse(csEsConfig.ExternalURL)
	if err != nil {
		return err
	}
	a1URL, err := url.Parse(a1EsConfig.NginxProxyURL)
	if err != nil {
		return err
	}

	URLsMatch := ((csURL.Host == a1URL.Host) &&
		(csURL.Port() == a1URL.Port()) &&
		(path.Clean(csURL.EscapedPath()) == path.Clean(a1URL.EscapedPath())))

	erchefUsesElastic := (csErchefConfig.SearchProvider == "elasticsearch")

	searchQueueModeIsBatch := (csErchefConfig.SearchQueueMode == "batch")

	if esEnabled && esExternal && URLsMatch && erchefUsesElastic && searchQueueModeIsBatch {
		return nil
	}
	_, err = c.Msgs.WriteString(invalidChefServerSearchConfigMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

func (c *CompatChecker) ChefServerPostgresConfigValid(csPgConfig *CSRPostgreSQL, a1PgConfig *DeliveryRunningPostgreSQL) error {
	pgEnabled := csPgConfig.Enable
	pgExternal := csPgConfig.External
	VIPsMatch := (csPgConfig.Vip == a1PgConfig.Vip)
	PortsMatch := (csPgConfig.Port == a1PgConfig.Port)
	if pgEnabled && pgExternal && VIPsMatch && PortsMatch {
		return nil
	}

	_, err := c.Msgs.WriteString(invalidChefServerPostgresConfigMsg)
	if err != nil {
		return err
	}
	c.Failures++
	return nil
}

func (c *CompatChecker) ChefServerBookshelfConfigValid(csBookshelfConfig *Bookshelf) error {
	bookshelfEnabled := csBookshelfConfig.Enable

	bookshelfUsesSQL := (csBookshelfConfig.StorageType == "sql")

	if bookshelfEnabled && bookshelfUsesSQL {
		return nil
	}

	_, err := c.Msgs.WriteString(invalidChefServerBookshelfConfigMsg)
	if err != nil {
		return err
	}
	c.Failures++

	return nil
}

func (c *CompatChecker) UnsupportedCSAddOnsNotUsed(omnibusRoot string) error {
	for _, addOn := range chefServerAddOns {
		addOnPath := path.Join(omnibusRoot, addOn)

		_, err := os.Stat(addOnPath)
		if os.IsNotExist(err) {
			continue
		}
		msg := fmt.Sprintf(chefServerAddOnInUseFmt, addOn)
		_, err = c.Msgs.WriteString(msg)
		if err != nil {
			return err
		}
		c.Failures++

	}

	return nil
}

func (c *CompatChecker) OcIDNotUsed(ocIDConfig *OcID) error {
	ocIDApps := ocIDConfig.Applications
	if len(ocIDApps) != 0 {
		_, err := c.Msgs.WriteString(ocIDInUseMsg)
		if err != nil {
			return err
		}
		c.Failures++
	}
	return nil
}

func directoryEmpty(dirName string) (bool, error) {
	// Assumes directory exists
	f, err := os.Open(dirName)
	if err != nil {
		return false, err
	}
	defer f.Close()

	_, err = f.Readdirnames(1)
	if err == io.EOF {
		return true, nil
	}
	return false, err
}
