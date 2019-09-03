package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"strconv"
	"strings"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/gatherlogs"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/preflight"
	"github.com/chef/automate/lib/platform/pg"

	"github.com/chef/automate/lib/io/chunks"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// GatherLogs collects logs and other data, packages them in a compressed
// archive, and returns the archive
func (s *server) GatherLogs(ctx context.Context, req *api.GatherLogsRequest,
) (*api.GatherLogsResponse, error) {
	// path we do all of our work in
	stagingDir := stagingDir(s.serverConfig)

	// disposable directory we collect data into
	archiveRoot, err := createTempDir(stagingDir, "log-gathering")
	if err != nil {
		return nil, err
	}
	// clean up our staging area
	defer os.RemoveAll(archiveRoot)

	// binPaths defined where we can find the executables required for our log
	// gathering operations, rather than relying on PATH
	// TODO: make this s.serverConfig.BinPaths
	binPaths := map[string]string{
		"cp":   path.Join(s.serverConfig.CoreutilsPath, "cp"),
		"find": s.serverConfig.FindPath,
		"tar":  s.serverConfig.TarPath,
	}

	// initialize log gathering configuration
	g := gatherlogs.NewGatherer(
		stagingDir,
		archiveRoot,
		s.deployment.Config.Global.V1.Fqdn.Value,
		binPaths,
		time.Now(),
	)
	if err = g.CreateBundleDir(); err != nil {
		return nil, err
	}

	// populate bundle dir

	// Chef Automate
	g.AddOther("chef-automate_preflight-check", gatherPreflightCheck(s))
	g.AddOther("chef-automate_service-versions", gatherServiceVersions(ctx, s))
	g.AddOther("chef-automate_status", gatherStatusCheck(ctx, s))
	g.AddOther("chef-automate_release_versions", gatherVersionsFromCurrentManifest(ctx, s))
	g.AddOther("chef-automate_current_manifest", gatherCurrentManifest(ctx, s))

	if req.LogLines > 0 {
		logLinesStr := strconv.FormatUint(req.LogLines, 10)
		g.AddCommand("journalctl_chef-automate", "journalctl", "--utc", "-u", "chef-automate", "-n", logLinesStr)
	} else {
		g.AddCommand("journalctl_chef-automate", "journalctl", "--utc", "-u", "chef-automate")
	}

	g.AddOther("license", collectLicense(ctx, s))

	// Habitat
	g.AddCommand("hab_version", "hab", "--version")
	g.AddCommand("hab_sup_version", "hab", "sup", "--version")
	g.AddURL("hab_services", "http://localhost:9631/services")
	g.AddURL("hab_census", "http://localhost:9631/census")
	g.AddURL("hab_butterfly", "http://localhost:9631/butterfly")
	g.AddCommand("ulimit_a_hab", "su", "-m", "hab", "-c", "ulimit -a")

	// Habitat applications
	g.AddCommand("find_hab_pkgs", "find", strings.Split("/hab/pkgs -maxdepth 4 -mindepth 4 -type d -ls", " ")...)
	g.AddCopiesFromPath("user.toml", "/hab/user")
	g.AddCopiesFromPath("config", "/hab/svc")
	g.AddCopiesFromPath("logs", "/hab/svc")

	// System info
	g.AddCommand("df_h", "df", "-h")
	g.AddCommand("df_i", "df", "-i")
	g.AddCommand("df_k", "df", "-k")
	g.AddCommand("free_m", "free", "-m")
	// Use a package whose `ps` understands `fauxww`
	g.AddCommand("ps_fauxww", "hab", "pkg", "exec", "core/procps-ng", "ps", "fauxww")
	g.AddCommand("umask", "bash", "-c", "umask")
	g.AddCommand("uname_a", "uname", "-a")
	g.AddCommand("uptime", "uptime")
	g.AddCommand("sysctl_a", "sysctl", "-a")
	g.AddCommand("dmesg", "dmesg", "-T")
	g.AddCopy("/proc/cpuinfo")
	g.AddCopy("/proc/meminfo")
	g.AddCopy("/proc/version")
	g.AddCopy("/etc/resolv.conf")
	g.AddCopy("/etc/hosts")
	g.AddCopy("/proc/sys/crypto/fips_enabled")
	g.AddCopiesFromPath("syslog", "/var/log")
	g.AddCopiesFromPath("messages", "/var/log")
	// Distro version/release of the underlying operating system.
	// If a distro uses systemd, it provides distro information in
	// /etc/os-release
	// http://0pointer.de/blog/projects/os-release.html
	g.AddCopy("/etc/os-release")
	// Copying the old distro release/version files in addition to what is provided
	// in /etc/os-release in case the contents are not the same.
	// Files via mixlib-install's platform detection script.
	// https://github.com/chef/mixlib-install/blob/fe6ecabe54476e0dd6bd7d7553dba1f85337f105/lib/mixlib/install/generator/bourne/scripts/platform_detection.sh
	g.AddCopy("/etc/lsb-release")
	g.AddCopy("/etc/debian_version")
	g.AddCopy("/etc/redhat-release")
	g.AddCopy("/etc/system-release")
	g.AddCopy("/etc/debian-release")
	g.AddCopy("/etc/SuSE-release")

	// network
	hostname := g.FetchHostname()
	hostnameFqdn := g.FetchHostname("--fqdn")

	g.AddCommand("dig-fqdn", "dig", hostnameFqdn)
	g.AddCommand("hostname", "hostname")
	// Use a package whose `hostname` understands `--fqdn`
	g.AddCommand("hostname_--fqdn", "hab", "pkg", "exec", "core/net-tools", "hostname", "--fqdn")
	g.AddCommand("ip_addr_show", "ip", "addr", "show")
	g.AddCommand("ping_-c_2_hostname", "ping", "-c", "2", hostname)
	g.AddCommand("ping_-c_2_fqdn", "ping", "-c", "2", hostnameFqdn)
	g.AddCommand("ss", "ss", "--options", "--numeric", "--tcp", "--all", "--processes")

	// automate-gateway metrics
	// FIXME: Move this to using config.
	g.AddURL("automate-gateway_metrics", "https://localhost:2000/metrics")

	// Elasticsearch
	// FIXME: Move this to using config.
	elasticsearchURL := "http://localhost:10141"
	g.AddURL("elasticsearch_cat_health", elasticsearchURL+"/_cat/health?v")
	g.AddURL("elasticsearch_cluster_health", elasticsearchURL+"/_cluster/health?human&pretty")
	g.AddURL("elasticsearch_cluster_pending_tasks", elasticsearchURL+"/_cluster/pending_tasks?human&pretty")
	g.AddURL("elasticsearch_cluster_settings", elasticsearchURL+"/_cluster/settings?human&pretty")
	g.AddURL("elasticsearch_cluster_state", elasticsearchURL+"/_cluster/state?human&pretty")
	g.AddURL("elasticsearch_cluster_stats", elasticsearchURL+"/_cluster/stats?human&pretty")
	g.AddURL("elasticsearch_nodes", elasticsearchURL+"/_nodes?human&pretty")
	g.AddURL("elasticsearch_nodes_all", elasticsearchURL+"/_nodes/_all?human&pretty")
	g.AddURL("elasticsearch_nodes_health", elasticsearchURL+"/_nodes/health?human&pretty")
	g.AddURL("elasticsearch_nodes_stats", elasticsearchURL+"/_nodes/stats?human&pretty")

	// Postgresql
	// FIXME: Move this to using config.
	connInfo := pg.A2ConnInfo{
		Host:  "localhost",
		Port:  5432,
		User:  "automate",
		Certs: pg.A2SuperuserCerts,
	}
	connectionURI := connInfo.ConnURI("template1")

	pcmd := pg.PSQLCmd()[0]

	activityArgs := pg.PSQLCmd()[1:]
	activityArgs = append(activityArgs, connectionURI, "-c", "SELECT CURRENT_TIMESTAMP; SELECT * FROM pg_stat_activity;")
	g.AddCommand("pg_stat_activity", pcmd, activityArgs...)

	sqitchArgs := pg.PSQLCmd()[1:]
	sqitchArgs = append(sqitchArgs, connectionURI, "-c", "SELECT * FROM sqitch.tags;")
	g.AddCommand("sqitch_tags", pcmd, sqitchArgs...)

	tokenArgs := pg.PSQLCmd()[1:]
	tokenArgs = append(tokenArgs, connInfo.ConnURI("chef_authn_service"),
		"-c", "SELECT active, count(*) FROM chef_authn_tokens GROUP BY active;")
	g.AddCommand("api_tokens", pcmd, tokenArgs...)

	g.ExecuteAll()

	bundleInfo, err := g.CreateBundleFile()
	if err != nil {
		return nil, err
	}

	return &api.GatherLogsResponse{
		BundleName:     bundleInfo.Name,
		BundleChecksum: bundleInfo.Checksum,
		BundleSize:     bundleInfo.Size,
	}, nil
}

func (s *server) GatherLogsDownload(
	req *api.GatherLogsDownloadRequest,
	stream api.Deployment_GatherLogsDownloadServer,
) error {
	bundlePath := path.Join(stagingDir(s.serverConfig), req.BundleName)

	defer func() {
		err := os.Remove(bundlePath)
		if err != nil {
			log.WithError(err).Warn("Failed to remove support bundle file.")
		}
	}()

	// Open our file to transfer
	// We should potentially compare value given against archive files in directory
	//   to prevent path traversal exploits
	file, err := os.Open(bundlePath)
	if err != nil {
		return err
	}
	defer file.Close()

	buffer := make([]byte, defaultChunkSize)
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.GatherLogsDownloadResponse{Data: p})
	})
	_, err = io.CopyBuffer(writer, file, buffer)
	return err
}

// collectLicense returns a function that requests the loaded license and
// writes it to a file, or if one isn't loaded, writes that instead.
func collectLicense(ctx context.Context, server *server) func() ([]byte, error) {
	return func() ([]byte, error) {
		var out string

		license, err := server.License(ctx)
		if err != nil {
			if status.Code(err) == codes.NotFound {
				out = "License Not Set"
			} else {
				out = "Unknown error retrieving license data"
			}
		} else {
			out = license.PrettyPrint()
		}

		return []byte(out + "\n"), nil
	}
}

func gatherPreflightCheck(_ *server) func() ([]byte, error) {
	return func() ([]byte, error) {
		out, err := preflight.RunDeployPreflightCheck(preflight.DeployPreflightCheckOptions{
			Airgap: airgap.AirgapInUse(),
		})
		if err != nil {
			return []byte{}, err
		}
		return []byte(out), err
	}
}

func gatherStatusCheck(ctx context.Context, s *server) func() ([]byte, error) {
	return func() ([]byte, error) {
		resp, err := s.Status(ctx, &api.StatusRequest{})
		if err != nil {
			return []byte{}, err
		}
		return []byte(resp.ServiceStatus.FormatStatus()), err
	}
}

// gatherCurrentManifest, gatherVersionsFromCurrentManifest, and gatherServiceVersions
// collect similar information and present it in different ways for different
// audiences/purposes. We should think about if/how to consolidate.
func gatherCurrentManifest(ctx context.Context, s *server) func() ([]byte, error) {
	return func() ([]byte, error) {
		resp, err := s.CurrentReleaseManifest(ctx, &api.CurrentReleaseManifestRequest{})
		if err != nil {
			return nil, err
		}
		s := fmt.Sprintf("%s", resp.Json)
		return []byte(s), nil
	}
}

func gatherVersionsFromCurrentManifest(ctx context.Context, s *server) func() ([]byte, error) {
	return func() ([]byte, error) {
		resp, err := s.CurrentReleaseManifest(ctx, &api.CurrentReleaseManifestRequest{})
		if err != nil {
			return nil, err
		}
		manifest := &manifest.A2{}
		err = json.Unmarshal(resp.Json, manifest)
		if err != nil {
			return nil, err
		}
		found, cliPkg := manifest.PackageForServiceName("automate-cli")
		if !found {
			log.Warn("Did not find automate-cli in current manifest")
		}
		releaseFmt := `Server release: %s, Server SHA: %s, Client release: %s
`
		s := fmt.Sprintf(releaseFmt, manifest.Version(), manifest.SHA(), cliPkg.Release())
		return []byte(s), nil
	}
}

func gatherServiceVersions(ctx context.Context, s *server) func() ([]byte, error) {
	return func() ([]byte, error) {
		resp, err := s.ServiceVersions(ctx, &api.ServiceVersionsRequest{})
		if err != nil {
			return []byte{}, err
		}
		buf := new(bytes.Buffer)
		for _, s := range resp.Services {
			buf.WriteString(fmt.Sprintf("%s/%s %s %s\n", s.Origin, s.Name, s.Version, s.Release))
		}
		return buf.Bytes(), nil
	}
}

func stagingDir(serverConfig *Config) string {
	if serverConfig.StagingDir != "" {
		if err := os.MkdirAll(serverConfig.StagingDir, 0700); err != nil {
			log.WithFields(
				log.Fields{"error": err, "staging_dir": serverConfig.StagingDir},
			).Warn("Failed to create non-default staging directory.")
		} else {
			return serverConfig.StagingDir
		}
	}

	return DataDir
}

func createTempDir(parent, prefix string) (string, error) {
	tempDir, err := ioutil.TempDir(parent, prefix)
	if err != nil {
		log.WithError(err).Error("Failed to create temporary directory.")

		return "", err
	}

	return tempDir, nil
}
