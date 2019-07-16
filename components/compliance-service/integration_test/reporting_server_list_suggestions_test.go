package integration_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"golang.org/x/net/context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestReportingListSuggestionsFiltering(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()

	cases := []struct {
		description   string
		summaries     []*relaxting.ESInSpecSummary
		request       reporting.SuggestionRequest
		expectedTerms []string
	}{
		// organization
		{
			description: "Only two orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
				{
					NodeID:           "3",
					OrganizationName: "1/75th Airborne Rangers",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
				{
					NodeID:           "3",
					OrganizationName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
				{
					NodeID:           "3",
					OrganizationName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// inspec_version
		{
			description: "Only two versions are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "3.1",
			},
			expectedTerms: []string{"3.1.0", "3.1.1"},
		},
		{
			description: "All inspec_version are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "",
			},
			expectedTerms: []string{"3.1.0", "3.1.1", "4.1.0"},
		},
		{
			description: "No inspec_version are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "1.0",
			},
			expectedTerms: []string{},
		},

		// chef_server
		{
			description: "Only two chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
				{
					NodeID:     "3",
					SourceFQDN: "bob",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
				{
					NodeID:     "3",
					SourceFQDN: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
				{
					NodeID:     "3",
					SourceFQDN: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// chef_tags
		{
			description: "Only two chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org3"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"bob"},
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2", "org3", "org4"},
		},
		{
			description: "All chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org5"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"org3", "org6"},
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3", "org4", "org5", "org6"},
		},
		{
			description: "No chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org5"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"org3", "org6"},
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// policy_group
		{
			description: "Only two policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
				{
					NodeID:      "3",
					PolicyGroup: "bob",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
				{
					NodeID:      "3",
					PolicyGroup: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
				{
					NodeID:      "3",
					PolicyGroup: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// policy_name
		{
			description: "Only two policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
				{
					NodeID:     "3",
					PolicyName: "bob",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
				{
					NodeID:     "3",
					PolicyName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
				{
					NodeID:     "3",
					PolicyName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "bob",
			},
			expectedTerms: []string{},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			_, err := suite.InsertInspecSummaries(test.summaries)
			require.NoError(t, err)
			defer suite.DeleteAllDocuments()

			response, err := server.ListSuggestions(ctx, &test.request)
			require.NoError(t, err)
			require.NotNil(t, response)

			actualTerms := make([]string, len(response.Suggestions))
			for i, suggestion := range response.Suggestions {
				actualTerms[i] = suggestion.Text
			}

			assert.ElementsMatch(t, test.expectedTerms, actualTerms)
		})
	}
}

// The terms (sorted alphabetically) after the first 500 are not included in the suggestions.
func TestReportingListSuggestionsLargeArrayValues(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()
	terms := []string{ //517
		"windows.allow_reboot_on_failure",
		"nginx.client_max_body_size",
		"yum.epel-source.failovermethod",
		"yum.main.groupremove_leaf_only",
		"yum.main.installroot",
		"yum.main.keepcache",
		"jenkins.java",
		"bluepill.conf_dir",
		"yum.main.installonly_limit",
		"apt.unattended_upgrades.remove_unused_dependencies",
		"yum.main.debuglevel",
		"omnibus.toolchain_version",
		"nginx.echo.version",
		"nginx.geoip.lib_checksum",
		"yum.main.mirrorlist_expire",
		"yum.main.enable_group_conditionals",
		"windows.allow_pending_reboots",
		"apt.unattended_upgrades.automatic_reboot",
		"jenkins.master.version",
		"rsyslog.service_name",
		"nginx.naxsi.version",
		"nginx.upload_progress.url",
		"nginx.source.version",
		"seven_zip.url",
		"nginx.upload_progress.checksum",
		"nginx.gzip_buffers",
		"nginx.openssl_source.url",
		"yum.epel.gpgcheck",
		"rsyslog.rate_limit_burst",
		"nginx.rate_limiting_zone_name",
		"nginx.passenger.packages.rhel",
		"nginx.socketproxy.root",
		"yum.epel-debuginfo.repositoryid",
		"yum.main.http_caching",
		"yum.main.sslclientcert",
		"runit.reload",
		"chef_handler.handler_path",
		"rsyslog.default_facility_logs.authpriv.*",
		"nginx.socketproxy.log_level",
		"yum.main.color_list_available_install",
		"jenkins.master.mirror",
		"nginx.geoip.country_dat_checksum",
		"nginx.upstart.respawn_limit",
		"nginx.tcp_nodelay",
		"nginx.version",
		"nginx.source.default_configure_flags",
		"yum.epel-testing.gpgkey",
		"apt.cacher-client.restrict_environment",
		"yum.main.color_list_available_reinstall",
		"yum.epel-testing-debuginfo.gpgkey",
		"opscode-ci.master.artifacts_to_keep",
		"rsyslog.remote_logs",
		"rsyslog.tls_auth_mode",
		"rsyslog.preserve_fqdn",
		"yum.main.color_list_installed_older",
		"authorization.sudo.groups",
		"nginx.passenger.nodejs",
		"nginx.pagespeed.url",
		"yum.epel-debuginfo.managed",
		"yum.main.tolerant",
		"homebrew.taps",
		"jenkins.executor.proxy",
		"yum.epel.repositoryid",
		"nginx.geoip.lib_url",
		"yum.main.syslog_facility",
		"nginx.naxsi.url",
		"nginx.sendfile",
		"yum.main.ssl_check_cert_permissions",
		"nginx.source.url",
		"yum.epel-source.make_cache",
		"yum.epel-source.gpgkey",
		"seven_zip.checksum",
		"xml.packages",
		"jenkins.master.log_directory",
		"nginx.default.modules",
		"rsyslog.protocol",
		"nginx.source.use_existing_user",
		"omnibus.install_dir",
		"nginx.source.checksum",
		"yum.main.color_update_installed",
		"rsyslog.default_facility_logs.*.info;mail.none;authpriv.none;cron.none",
		"nginx.keepalive",
		"yum.epel.failovermethod",
		"homebrew.owner",
		"jenkins.master.source",
		"nginx.worker_connections",
		"yum.epel-source.repositoryid",
		"yum.epel-source.gpgcheck",
		"yum.main.history_list_view",
		"yum.epel-testing-debuginfo.make_cache",
		"authorization.sudo.passwordless",
		"bluepill.state_dir",
		"yum.main.reposdir",
		"yum.main.protected_packages",
		"yum.epel-testing-source.managed",
		"yum.epel-testing-debuginfo.enabled",
		"yum-epel.repositories",
		"nginx.default_site_enabled",
		"yum.main.syslog_ident",
		"rsyslog.default_file_template",
		"nginx.set_misc.version",
		"yum.main.color",
		"rsyslog.log_dir",
		"yum.epel-testing-source.make_cache",
		"apt.compiletime",
		"nginx.socketproxy.logname",
		"nginx.lua.version",
		"nginx.keepalive_timeout",
		"nginx.disable_access_log",
		"opscode-ci.master.port",
		"nginx.default_root",
		"nginx.install_method",
		"yum.main.exactarch",
		"yum.main.recent",
		"authorization.sudo.setenv",
		"jenkins.master.checksum",
		"jenkins.master.jenkins_args",
		"rsyslog.server_search",
		"rsyslog.default_log_dir",
		"yum.epel-debuginfo.make_cache",
		"nginx.devel.url",
		"yum.main.pluginconfpath",
		"jenkins.master.home",
		"rsyslog.tls_ca_file",
		"opscode-ci.email.list_id",
		"opscode-ci.group",
		"nginx.devel.version",
		"yum.epel-source.mirrorlist",
		"opscode-ci.monitoring.jmx_port",
		"nginx.gzip_comp_level",
		"nginx.gzip_disable",
		"yum.main.max_retries",
		"yum.main.exclude",
		"yum.epel-testing-source.failovermethod",
		"authorization.sudo.agent_forwarding",
		"git.url",
		"apt.unattended_upgrades.allowed_origins",
		"jenkins.master.listen_address",
		"rsyslog.logs_to_forward",
		"nginx.source.conf_path",
		"yum.main.tsflags",
		"jenkins.master.use_system_accounts",
		"nginx.geoip.city_dat_url",
		"nginx.source.prefix",
		"omnibus.build_user",
		"nginx.passenger.gem_binary",
		"nginx.luajit.version",
		"nginx.auth_request.url",
		"authorization.sudo.prefix",
		"xml.compiletime",
		"jenkins.master.port",
		"yum.main.gpgcheck",
		"yum.main.commands",
		"jenkins.master.install_method",
		"rsyslog.max_message_size",
		"rsyslog.use_relp",
		"nginx.set_misc.checksum",
		"yum.main.color_list_available_downgrade",
		"homebrew.auto-update",
		"build-essential.msys2.path",
		"nginx.gzip",
		"opscode-ci.master.pipeline_types",
		"nginx.upload_progress.javascript_output",
		"yum.main.deltarpm",
		"nginx.psol.url",
		"bluepill.logfile",
		"homebrew.casks",
		"rsyslog.default_facility_logs.uucp,news.crit",
		"nginx.passenger.max_instances_per_app",
		"nginx.pagespeed.version",
		"nginx.upload_progress.zone_size",
		"nginx.pagespeed.packages.debian",
		"bluepill.group",
		"java.jdk_version",
		"aws.right_aws_version",
		"nginx.client_body_buffer_size",
		"yum.main.multilib_policy",
		"nginx.server_names_hash_bucket_size",
		"opscode-ci.master.executors",
		"yum.epel-source.description",
		"yum.main.color_list_available_upgrade",
		"yum.main.persistdir",
		"yum.main.logfile",
		"yum.epel-testing.failovermethod",
		"yum.epel-testing.description",
		"opscode-ci.master.host_name",
		"omnibus.toolchain_name",
		"wix.home",
		"nginx.passenger.buffer_response",
		"apt.cacher_ssl_support",
		"nginx.proxy_read_timeout",
		"nginx.gzip_types",
		"runit.lsb_init_dir",
		"packagecloud.gpg_key_path",
		"nginx.status.port",
		"nginx.rate_limit",
		"yum.main.proxy",
		"yum.main.history_record",
		"opscode-ci.artifactory.endpoint",
		"rsyslog.tls_certificate_file",
		"rsyslog.config_prefix",
		"nginx.log_dir_perm",
		"nginx.luajit.checksum",
		"nginx.script_dir",
		"yum.epel-debuginfo.gpgcheck",
		"yum.main.installonlypkgs",
		"windows-sdk.install_path",
		"nginx.passenger.max_pool_size",
		"nginx.set_misc.url",
		"nginx.syslog.git_revision",
		"runit.sv_bin",
		"yum.epel-testing-debuginfo.repositoryid",
		"windows.reboot_timeout",
		"rsyslog.enable_imklog",
		"git.prefix",
		"authorization.sudo.users",
		"wix.checksum",
		"jenkins.master.endpoint",
		"yum.main.proxy_password",
		"yum.epel-testing-source.enabled",
		"authorization.sudo.include_sudoers_d",
		"authorization.sudo.command_aliases",
		"apt.unattended_upgrades.mail",
		"ohai.hints_path",
		"bluepill.pid_dir",
		"yum.epel-testing-debuginfo.managed",
		"nginx.ulimit",
		"nginx.log_dir",
		"nginx.luajit.url",
		"nginx.access_log_options",
		"yum.main.errorlevel",
		"yum.epel-testing-debuginfo.description",
		"omnibus.build_user_group",
		"rsyslog.server",
		"yum.epel.description",
		"yum.main.cachedir",
		"nginx.multi_accept",
		"yum.main.sslcacert",
		"yum.main.username",
		"rsyslog.default_facility_logs.cron.*",
		"yum.epel-source.managed",
		"yum.main.showdupesfromrepos",
		"nginx.upstart.runlevels",
		"omnibus.git_checksum",
		"authorization.sudo.env_keep_subtract",
		"apt.unattended_upgrades.update_package_lists",
		"xml.nokogiri.use_system_libraries",
		"aws.databag_name",
		"nginx.devel.checksum",
		"nginx.headers_more.source_url",
		"apt.periodic_update_min_delay",
		"yum.epel.enabled",
		"yum.main.skip_broken",
		"yum.main.plugins",
		"opscode-ci.email.system_address",
		"omnibus.build_user_home",
		"packagecloud.hostname_override",
		"nginx.echo.url",
		"nginx.pagespeed.packages.rhel",
		"nginx.gzip_http_version",
		"yum.main.color_search_match",
		"yum.main.proxy_username",
		"chef-ingredient.mixlib-install.git_ref",
		"nginx.passenger.install_rake",
		"nginx.types_hash_max_size",
		"nginx.port",
		"git.server.export_all",
		"build-essential.compile_time",
		"authorization.sudo.sudoers_defaults",
		"nginx.repo_source",
		"yum.main.history_record_packages",
		"nginx.upstart.foreground",
		"nginx.dir",
		"opscode-ci.master.jnlp_port",
		"omnibus.base_dir",
		"runit.start",
		"rsyslog.rate_limit_interval",
		"yum.main.releasever",
		"yum.main.password",
		"seven_zip.package_name",
		"runit.stop",
		"nginx.tcp_nopush",
		"nginx.passenger.version",
		"yum.main.color_update_local",
		"nginx.echo.checksum",
		"nginx.source.sbin_path",
		"nginx.passenger.spawn_method",
		"nginx.lua.url",
		"xml.nokogiri.version",
		"windows.rubyzipversion",
		"jenkins.master.host",
		"nginx.worker_processes",
		"nginx.keepalive_requests",
		"nginx.gzip_vary",
		"yum.main.syslog_device",
		"apt.unattended_upgrades.automatic_reboot_time",
		"route53.fog_version",
		"jenkins.master.repository_keyserver",
		"rsyslog.repeated_msg_reduction",
		"apt.compile_time_update",
		"packagecloud.default_type",
		"packagecloud.base_repo_path",
		"jenkins.master.runit.sv_timeout",
		"nginx.variables_hash_max_size",
		"nginx.headers_more.version",
		"jenkins.master.user",
		"nginx.socketproxy.app_owner",
		"yum.epel-testing.mirrorlist",
		"runit.sv_dir",
		"yum.epel-testing-source.mirrorlist",
		"apt.cacher_port",
		"apt.launchpad_api_version",
		"rsyslog.enable_tls",
		"rsyslog.user",
		"rsyslog.server_ip",
		"rsyslog.modules",
		"yum.epel.make_cache",
		"yum.main.diskspacecheck",
		"yum.epel-testing-debuginfo.mirrorlist",
		"nginx.geoip.path",
		"yum.main.clean_requirements_on_remove",
		"nginx.passenger.pool_idle_time",
		"apt.confd.install_suggests",
		"nginx.naxsi.checksum",
		"yum.epel.managed",
		"apt.unattended_upgrades.dl_limit",
		"yum.epel-testing-debuginfo.gpgcheck",
		"apt.cacher_interface",
		"runit.executable",
		"nginx.passenger.ruby",
		"nginx.configure_flags",
		"yum.main.color_list_installed_reinstall",
		"yum.main.obsoletes",
		"yum.main.repo_gpgcheck",
		"opscode-ci.master.days_to_keep_builds",
		"jenkins.executor.private_key",
		"rsyslog.port",
		"yum.main.localpkg_gpgcheck",
		"yum.epel-testing-source.gpgkey",
		"opscode-ci.user",
		"opscode-ci.monitoring.jmx_hostname",
		"nginx.pid",
		"yum.epel-debuginfo.description",
		"omnibus.ruby_version",
		"git.use_pcre",
		"yum.main.overwrite_groups",
		"yum.epel-testing-debuginfo.failovermethod",
		"yum.epel-debuginfo.mirrorlist",
		"rsyslog.relp_port",
		"apt.key_proxy",
		"ohai.plugin_path",
		"nginx.init_style",
		"nginx.rate_limiting_backoff",
		"apt.unattended_upgrades.install_on_shutdown",
		"jenkins.master.repository_key",
		"rsyslog.tls_key_file",
		"nginx.event",
		"nginx.variables_hash_bucket_size",
		"bluepill.init_dir",
		"yum.main.mdpolicy",
		"authorization.sudo.env_keep_add",
		"dmg.cache_dir",
		"rsyslog.use_local_ipv4",
		"nginx.package_name",
		"yum.main.alwaysprompt",
		"apt.confd.install_recommends",
		"aws.databag_entry",
		"nginx.geoip.lib_version",
		"yum.main.color_update_remote",
		"opscode-ci.master.days_to_keep_artifacts",
		"ohai.plugins.ohai",
		"nginx.upstream_repository",
		"git.version",
		"yum.main.timeout",
		"apt.unattended_upgrades.mail_only_on_error",
		"runit.service_dir",
		"rsyslog.default_facility_logs.local7.*",
		"nginx.gzip_min_length",
		"nginx.binary",
		"yum.main.rpmverbosity",
		"homebrew.formulas",
		"omnibus.git_version",
		"nginx.passenger.packages.fedora",
		"nginx.worker_rlimit_nofile",
		"nginx.group",
		"yum.epel-source.enabled",
		"yum.epel-debuginfo.failovermethod",
		"yum.epel-testing-source.gpgcheck",
		"yum.epel-testing-source.description",
		"yum.epel-testing.make_cache",
		"omnibus.build_user_password",
		"yum.main.throttle",
		"nginx.gzip_proxied",
		"git.server.base_path",
		"yum.main.color_list_installed_newer",
		"runit.prefer_local_yum",
		"dmg.base_dir",
		"yum.main.bandwidth",
		"apt.unattended_upgrades.minimal_steps",
		"jenkins.master.jvm_options",
		"nginx.passenger.root",
		"bluepill.use_rsyslog",
		"yum.main.sslverify",
		"runit.chpst_bin",
		"jenkins.master.group",
		"nginx.passenger.min_instances",
		"nginx.headers_more.source_checksum",
		"nginx.source.modules",
		"nginx.geoip.country_dat_url",
		"yum.main.group_package_types",
		"nginx.user",
		"nginx.accept_mutex_delay",
		"nginx.auth_request.checksum",
		"bluepill.bin",
		"yum.epel.gpgkey",
		"yum.main.keepalive",
		"yum.main.protected_multilib",
		"yum.main.pluginpath",
		"nginx.error_log_options",
		"nginx.underscores_in_headers",
		"yum.epel-debuginfo.gpgkey",
		"yum.epel-testing.enabled",
		"yum.epel-testing.repositoryid",
		"apt.caching_server",
		"nginx.gzip_static",
		"yum.epel-debuginfo.enabled",
		"yum.main.distroverpkg",
		"apt.unattended_upgrades.package_blacklist",
		"jenkins.executor.timeout",
		"git.checksum",
		"rsyslog.priv_seperation",
		"nginx.lua.checksum",
		"rsyslog.per_host_dir",
		"rsyslog.group",
		"nginx.syslog.git_repo",
		"yum.main.assumeyes",
		"opscode-ci.user_home",
		"jenkins.master.repository",
		"nginx.geoip.enable_city",
		"nginx.enable_rate_limiting",
		"yum.epel-testing.gpgcheck",
		"rsyslog.default_facility_logs.mail.*",
		"nginx.geoip.city_dat_checksum",
		"yum.main.reset_nice",
		"apt.unattended_upgrades.auto_fix_interrupted_dpkg",
		"nginx.openssl_source.version",
		"yum.epel.mirrorlist",
		"rsyslog.default_remote_template",
		"rsyslog.default_facility_logs.*.emerg",
		"nginx.passenger.packages.debian",
		"nginx.passenger.max_requests",
		"nginx.upload_progress.zone_name",
		"yum.main.kernelpkgnames",
		"yum.epel-testing.managed",
		"opscode-ci.master.builds_to_keep",
		"apt.unattended_upgrades.enable",
		"rsyslog.high_precision_timestamps",
		"nginx.types_hash_bucket_size",
		"yum.main.path",
		"yum.main.metadata_expire",
		"yum.main.bugtracker_url",
		"yum.main.color_list_installed_extra",
		"yum.epel-testing-source.repositoryid",
		"wix.download_id",
		"nginx.large_client_header_buffers",
		"nginx.server_tokens",
		"yum.main.sslclientkey",
		"apt.cacher_dir",
		"authorization.sudo.sudoers_defaults",
		"authorization.sudo.include_sudoers_d",
		"omnibus.ruby_version",
		"omnibus.build_user_password",
		"jenkins.master.endpoint",
		"git.version",
		"git.checksum",
		"tags",
		"omnibus.build_user",
		"omnibus.build_user_home",
		"omnibus.build_user_group",
		"authorization.sudo.agent_forwarding",
		"opscode-ci.master.host_name",
		"opscode-ci.master.days_to_keep_artifacts",
		"opscode-ci.master.artifacts_to_keep",
		"opscode-ci.artifactory.endpoint",
		"opscode-ci.omnitruck.packages_bucket",
		"opscode-ci.omnitruck.metadata_bucket",
		"opscode-ci.master.pipeline_types",
		"opscode-ci::slave",
		"opscode-ci::_build_support",
		"chef-sugar::default",
		"opscode-ci::_platform_tweaks",
		"yum-epel::default",
		"opscode-ci::_migration",
		"omnibus::default",
		"omnibus::_common",
		"omnibus::_user",
		"omnibus::_omnibus_toolchain",
		"omnibus::_cacerts",
		"omnibus::_compile",
		"build-essential::default",
		"omnibus::_git",
		"omnibus::_ruby",
		"omnibus::_github",
		"omnibus::_libffi",
		"omnibus::_packaging",
		"omnibus::_selinux",
		"omnibus::_environment",
		"opscode-ci::_github",
		"opscode-ci::_ntp",
		"opscode-ci::_package_signing",
		"opscode-ci::_rubygems",
		"opscode-ci::_users",
		"opscode-ci::_sudo",
		"sudo::default",
		"opscode-ci::_java",
		"aws::default",
		"opscode-ci::_omnibus",
		"zum",
	}

	cases := []struct {
		description string
		summary     *relaxting.ESInSpecSummary
		request     *reporting.SuggestionRequest
	}{
		{
			description: "chef_tags",
			summary: &relaxting.ESInSpecSummary{
				ChefTags: terms,
			},
			request: &reporting.SuggestionRequest{
				Type: "chef_tags",
			},
		},
		{
			description: "recipe",
			summary: &relaxting.ESInSpecSummary{
				Recipes: terms,
			},
			request: &reporting.SuggestionRequest{
				Type: "recipe",
			},
		},
		{
			description: "roles",
			summary: &relaxting.ESInSpecSummary{
				Roles: terms,
			},
			request: &reporting.SuggestionRequest{
				Type: "role",
			},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {

			// Ingest the reports
			_, err := suite.InsertInspecSummaries([]*relaxting.ESInSpecSummary{test.summary})
			require.NoError(t, err)
			defer suite.DeleteAllDocuments()

			// last term alphabetically
			expectedTerm := "zum"

			// defaults to 10
			test.request.Size = 2
			// term to give suggestions for
			test.request.Text = "zum"

			// Make the request for suggestions
			response, err := server.ListSuggestions(ctx, test.request)
			require.NoError(t, err)
			require.NotNil(t, response)

			// abstract the terms from the response
			actualTerms := make([]string, len(response.Suggestions))
			for i, suggestion := range response.Suggestions {
				actualTerms[i] = suggestion.Text
			}

			// Check to see if last term alphabetically is a returned suggestion
			assert.True(t, contains(actualTerms, expectedTerm), "Suggestions does not contain term")
		})
	}
}

func contains(actual []string, expected string) bool {
	for _, a := range actual {
		if a == expected {
			return true
		}
	}
	return false
}

func TestReportingListSuggestions(t *testing.T) {
	reportFileName := "../ingest/examples/compliance-success-tiny-report.json"
	everythingCtx := contextWithProjects([]string{authzConstants.AllProjectsExternalID})

	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})

	n := 5

	reportIds := make([]string, n)

	for i := 0; i < n; i++ {
		err := suite.ingestReport(reportFileName, func(r *compliance.Report) {
			id := newUUID()

			r.Environment = id
			r.NodeName = id
			r.Platform.Name = id
			r.Profiles[0].Controls = r.Profiles[0].Controls[:1]
			r.Profiles[0].Controls[0].Id = id
			r.Profiles[0].Controls[0].Title = id
			r.Profiles = r.Profiles[:1]
			r.Profiles[0].Sha256 = id
			r.Profiles[0].Title = id
			r.Recipes = []string{id}
			r.ReportUuid = id
			r.Roles = []string{id}

			reportIds[i] = id
		})

		require.NoError(t, err)
	}

	defer suite.DeleteAllDocuments()

	waitFor(func() bool {
		response, _ := server.ListReports(everythingCtx, &reporting.Query{})

		return response != nil && len(response.Reports) == n
	})

	reportsProjects := map[string][]string{
		"project1": reportIds[1:3],
		"project2": reportIds[2:5],
		"project3": reportIds[3:],
	}

	projectRules := map[string]*iam_v2.ProjectRules{}
	for k, v := range reportsProjects {
		projectRules[k] = &iam_v2.ProjectRules{
			Rules: []*iam_v2.ProjectRule{
				{
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ROLES,
							Values:    v,
						},
					},
				},
			},
		}
	}

	// Send a project rules update event
	esJobID, err := suite.ingesticESClient.UpdateReportProjectsTags(everythingCtx, projectRules)
	assert.Nil(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceReportIndex()

	esJobID, err = suite.ingesticESClient.UpdateSummaryProjectsTags(everythingCtx, projectRules)
	assert.Nil(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceSummaryIndex()

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedIds     []string
	}{
		{
			description:     "user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     reportIds,
		},
		{
			description:     "user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     reportIds[1:3],
		},
		{
			description:     "user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     reportIds[1:5],
		},
		{
			description:     "user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:3],
		},
		{
			description:     "user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:5],
		},
		{
			description:     "user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
		{
			description:     "user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
	}

	suggestionTypes := []string{
		"environment",
		"node",
		"platform",
		"profile",
		"recipe",
		"role",
		"control",
	}

	for _, suggestionType := range suggestionTypes {
		for _, test := range successCases {
			t.Run(fmt.Sprintf("Projects: %q suggestions, %s", suggestionType, test.description), func(t *testing.T) {
				testCtx := contextWithProjects(test.allowedProjects)
				response, err := server.ListSuggestions(testCtx, &reporting.SuggestionRequest{Type: suggestionType})

				assert.NoError(t, err)
				require.NotNil(t, response)

				actualValues := make([]string, len(response.Suggestions))
				for i, suggestion := range response.Suggestions {
					actualValues[i] = suggestion.Text
				}

				assert.ElementsMatch(t, test.expectedIds, actualValues)
			})
		}
	}
}
