package a1stub

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"

	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/lib/platform/pg"
)

// A1 Directory constants
const (
	deliveryVarEtc     = "/var/opt/delivery/delivery/etc"
	deliveryEtc        = "/etc/delivery"
	a1ESDataDir        = "/var/opt/delivery/elasticsearch/data"
	a1ProfilesDir      = "/var/opt/delivery/compliance/profiles"
	a1ESBkupDir        = "/var/opt/delivery/elasticsearch_backups"
	a1ReaperDir        = "/var/opt/delivery/reaper_archive"
	a1NotificationsDir = "/var/opt/delivery/notifications"
	a1NginxCADir       = "/var/opt/delivery/nginx/ca"
	a1BackupDir        = "/var/opt/delivery/backups"
	chefServerEtc      = "/etc/opscode"
	chefServerVarDir   = "/var/opt/opscode"
)

// A2 File constants
const (
	a1NotificationsFile = "/var/opt/delivery/notifications/rule_store"
	a1SSLCertFmt        = "/var/opt/delivery/nginx/ca/%s.crt"
	a1SSLKeyFmt         = "/var/opt/delivery/nginx/ca/%s.key"
	a1BackupFile        = "/var/opt/delivery/backups/a1backup.rst"
)

const sslCert = `-----BEGIN CERTIFICATE-----
MIIDfTCCAmWgAwIBAgIBATANBgkqhkiG9w0BAQsFADBgMQswCQYDVQQGEwJVUzEW
MBQGA1UEChMNQ2hlZiBTb2Z0d2FyZTEWMBQGA1UECxMNQ2hlZiBBdXRvbWF0ZTEh
MB8GA1UEAxMYYXV0b21hdGUtZGVwbG95bWVudC50ZXN0MB4XDTE4MDQyNTEyNTcy
MFoXDTI4MDQyMjEyNTcyMFowYDELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUNoZWYg
U29mdHdhcmUxFjAUBgNVBAsTDUNoZWYgQXV0b21hdGUxITAfBgNVBAMTGGF1dG9t
YXRlLWRlcGxveW1lbnQudGVzdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC
ggEBALwCmfZIK7Ul5nEc9cPMHlBFM4graOqYrTyKJd2I0HERY7xgQNBEd6+wEXim
2EAaOOvVo+n+RniElNGFMz98hTDtzD4LcnS9VtYAWDkWQUrd+h0aQzHCbHIQFVO+
s66SVZOs74MCMYZUFIBkGlKTGOpRhTIeYPui3ud0mWNAuSczq/P+KtO/2WMU/vJn
0963wF/IakOiBaqMbd0vC2it1YqCkmdVH4Mt3tKbXzeLnQ6yViioNSHvQRLSVFKr
b1Re78+l7cJWBhRHepExhp3ot7d1IW4vC6xQkS41aYNLN+reD78w8ViDeGoAHU3K
xdt3GXlpe3LXPffPpIVYgZ4JX00CAwEAAaNCMEAwDgYDVR0PAQH/BAQDAgKkMB0G
A1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjAPBgNVHRMBAf8EBTADAQH/MA0G
CSqGSIb3DQEBCwUAA4IBAQB42/2YyrCD2WQgA05q/Qh4CYEkmeM9YzWWffkaBSj5
JFGueiIIAReIVsBqqrbHoDPHChnHiJdcFFX6b6EU4ZNR6ePi+PkPfRpeFGS12+AA
NW4zGs8ZGhiW/W64OVRqTa7YmSa9+Xnd+4zQx8ylIsb0BTIkYmSGbdbT4QS0nuAC
iCs0MY4OwGEfSUifHiQtbg0yVkMNKoF+5nAzrkPfCkSLnQK+7vlEquLJkUZJRnQQ
QNwrUmA1rvfoKkEEEKl69zhbg8RKiVpPQQiLgLs+sW+JYfNnu00D5ydFmmKWpRRL
+e1XyqxYbX8bwcovhLX9AJ0pXgr6/AyDMSwquDJwYgVi
-----END CERTIFICATE-----`

const sslKey = `-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEAvAKZ9kgrtSXmcRz1w8weUEUziCto6pitPIol3YjQcRFjvGBA
0ER3r7AReKbYQBo469Wj6f5GeISU0YUzP3yFMO3MPgtydL1W1gBYORZBSt36HRpD
McJschAVU76zrpJVk6zvgwIxhlQUgGQaUpMY6lGFMh5g+6Le53SZY0C5JzOr8/4q
07/ZYxT+8mfT3rfAX8hqQ6IFqoxt3S8LaK3VioKSZ1Ufgy3e0ptfN4udDrJWKKg1
Ie9BEtJUUqtvVF7vz6XtwlYGFEd6kTGGnei3t3Uhbi8LrFCRLjVpg0s36t4PvzDx
WIN4agAdTcrF23cZeWl7ctc998+khViBnglfTQIDAQABAoIBAQCvtjayaC0BNhSy
7n/6qBBEsQhCYV/CsUJm7C4LZ8x3f+sYqVSp0POS7RjHDBfbgSZlstto2059YFu9
naGR2gyY+AYEjqo/1J0NkHDjmZsYH3GcNWlLXgwAZmuoQvinVPhJ9jw60pf6lSY8
AFJhjjyg8eNsPdwiEcwFwYGBs4HX0eSdL4tqGR4KyptGhXI98I1vkNFWinPeA+qh
7SBBPEwrJmfMVQTMKxUw6wr8Cn1AVZPkqriXh3NFZScuk2WPRN9yv3Ol/294u4xc
6a4fWes2eJxhVXgertswEFvOITtNgpEyTkQnnD3tskV4ioKMcJgJsDs+cYn08mlL
6euhK/U5AoGBAPQhOweirLpJ+smepKTYgimsJw3nTTbOFCsOFCOyEirg9MdsymVr
RM5YNSVFCnpN7aVhCBCEFsnL1Bhf61vz3oGnWpOnMHnf9AFtTDkXxrNterHTxwro
H1QE8xYjIi6FdBPWsepwVUEh7PtQmlMh6pmuZQtdmcB5GFOTNWqVG7VTAoGBAMUm
1HSHLjAcABPEytRDRziG0M/lR9mAIHQSATEHu0zf5QASuLss1X1l9cu1z4Ow5i0u
HWzbCNy80GPW4v/yiw1EhyTAbaO1sEOQdMz0lSU7G6bbiqmWevE/YR8YYk7OxhDa
R4uPMgNQELZLVLsWFqysHMAz8tUqAKidmHaTZmTfAoGBAKKFYNPTcyPNTjxc9YSc
cgKmDZXO7vNWO8zx4WxmBnwvKCV/AjqEL9kilbbLAI+tQ1C+iOzd0oHixL65VGjx
r6YmsEDto3LvKXStu3min3AhqPWrY62aHAkGBoItP3sy8rSmT+kfgVIL8MTZwvMm
+BP5KmFnnYyp0Q0KShw7SafrAoGAQ0R5bv0EtGOJXzVhI0WFUnMlYhnQBxUhfZj7
ERibPqKSBxFcOHkWqr+UOxpgoIVGNBos7gxaHL7Lt5UFROEjsrY0CkUh457Fcngi
ch/tl5NwXuE/kZeHgTLn121BdbQ1fmCL0sFcBidWXGydj2v89pOPB8Fx1GuNZ/rF
n4exgy8CgYEAsxcQskZrJINp9kGGNGteZMFytzFGEPQ3m/FRBekVGBrEDGOLBqVA
jvJb/REvoFYPcNJVSz88P/rWgTI3UNCc34S12TqoRmjRpiu4ar7OkP1YxQ9dONi9
osQYj4UQRiMOwQfFY7R9rjdWHdrYeBS6BvWMu6NZY105upZfKVWhAD0=
-----END RSA PRIVATE KEY-----`

// StartTestHarness does what is necessary to be able to run the
// `upgrade-from-v1` code against a host that doesn't actually have A1
// installed. The test harness consists of these things:
// * sanity checks to make sure we actually don't have a1 installed, just in
//   case.
// * a set of stub binaries for commands such as `automate-ctl`;
//   StartTestHarness puts these in the PATH
// * a stub A1 API HTTP server.
// These components generally support invoking failure modes by setting the
// `FAILURE` environment variable to a value as defined in a1upgrade/upgrade.go
func StartTestHarness() error {
	if err := sanityCheck(); err != nil {
		return err
	}

	if a1upgrade.FailureInt != 0 {
		thPrintf("Running FAILURE scenario %v\n", a1upgrade.FailureInt)
	}

	if err := insertStubsInPATH(); err != nil {
		return err
	}

	if err := createDirLayout(); err != nil {
		return err
	}

	if err := createDataFiles(); err != nil {
		return err
	}

	if err := createA1EtcFiles(); err != nil {
		return err
	}

	if err := createA1VersionManifest(); err != nil {
		return err
	}

	if err := setupCerts(); err != nil {
		return err
	}

	go serveA1API()
	go serveRabbitAPI()
	servePostgreSQLStub()
	go serveESAPI()
	return nil
}

// CleanupTestHarness removes any tempfiles created by the test harness
func CleanupTestHarness() {
	cleanupCerts()
}

// sanityCheck verifies that the system we are running on does NOT have Chef
// Automate v1 installed. We want to be extra sure that a customer doesn't
// stumble on this and bork themselves or get confused by our weird error/debug
// messages
func sanityCheck() error {
	_, err := os.Stat("/opt/delivery")
	if os.IsNotExist(err) {
		return nil
	}

	_, err = os.Stat("/opt/delivery/.created-by-a2-self-test")
	if os.IsNotExist(err) {
		return errors.New("self test upgrade mode cannot be run on a system with Chef Automate v1")
	}

	return err
}

func insertStubsInPATH() error {
	chefAutomateCmdPath, err := os.Executable()
	if err != nil {
		return err
	}
	binDir := path.Dir(chefAutomateCmdPath)
	currentPath := os.Getenv("PATH")
	setPath := fmt.Sprintf("%s:%s", binDir, currentPath)
	thPrintf("setting PATH to %s\n", setPath)
	err = os.Setenv("PATH", setPath)
	if err != nil {
		return err
	}

	// Inject this to avoid calling hab pkg exec
	pg.PGDumpCmd = []string{"pg_dump"}

	automateCtlPath, _ := exec.LookPath("automate-ctl")
	pgDumpPath, _ := exec.LookPath("pg_dump")
	chefServerCtlPath, _ := exec.LookPath("chef-server-ctl")
	thPrintf("Using `chef-server-ctl` at %s\n", chefServerCtlPath)
	thPrintf("Using    `automate-ctl` at %s\n", automateCtlPath)
	thPrintf("Using         `pg_dump` at %s\n", pgDumpPath)
	return nil
}

func createDirLayout() error {
	dirsToMake := [...]string{
		deliveryEtc,
		deliveryVarEtc,
		a1ESDataDir,
		a1ProfilesDir,
		a1ESBkupDir,
		a1ReaperDir,
		a1NotificationsDir,
		a1NginxCADir,
		a1BackupDir,
		chefServerEtc,
		chefServerVarDir,
	}
	for _, dir := range dirsToMake {
		thPrintf("Creating directory %s\n", dir)
		if err := os.MkdirAll(dir, 0755); err != nil {
			return err
		}
	}
	return nil
}

func createDataFiles() error {
	filesToMake := [...]string{
		a1NotificationsFile,
		a1BackupFile,
	}
	for _, file := range filesToMake {
		thPrintf("Creating file %s\n", file)
		f, err := os.Create(file)
		if err != nil {
			return err
		}
		err = f.Close()
		if err != nil {
			return err
		}
	}

	return writeTestCerts()
}

var a1ManifestFmt = `automate %s

Component                   Installed Version   Version GUID                                                                                                                        Overridden From
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
appbundler                  v0.10.0             git:6582b68884453b5ca5c962458a0c08046c719558                                                                                        master
bundler                     1.8.32
bzip2                       1.0.6               sha256:a2848f34fcd5d6cf47def00461fcb528a0484d8edef8208d6d2e2909dc61d9cd
cacerts                     2018-01-17          sha256:defe310a0184a12e4b1b3d147f1d77395dd7a09e3428373d019bef5d542ceba3
chef                        v12.21.4            git:85500185dbafd6c91b3928e757e8b9435f79a8f9                                                                                        master
cmake                       3.4.3               md5:4cb3ff35b2472aae70f542116d616e63
common                      0.0.1
compliance-profiles         automate1.x         git:d088d8e1b78212ab2a616b1bce5bb5bfd2962594
compliance-profiles-dist    master              git:73ae34d7b5d995f4d2f2f469b60a64b072e38a27
config_guess                master              git:84f04b02a7e2fc8eaa9d52deee5f6d57b06fe447
cpanminus                   1.7004              md5:02fe90392f33a12979e188ea110dae67
curator                     5.1.1               md5:6de176b838c494815687dfe0dae2e5c3
curl                        7.56.0              sha256:f1bc17a7e5662dbd8d4029750a6dbdb72a55cf95826a270ab388b05075526104
delivery                    0.0.1
delivery-cookbooks          1.8.32
delivery-ctl                1.8.32
delivery-git-hooks          1.8.32
delivery-schema             1.8.32
delivery-scripts            1.8.32
delivery-server             1.8.32
delivery-vendor-cookbooks   1.8.32
delivery-web                1.8.32
elasticsearch               5.4.1               sha256:09d6422bd33b82f065760cd49a31f2fec504f2a5255e497c81050fd3dceec485
elasticsearch-plugins       5
elixir                      1.4.2               sha256:cb4e2ec4d68b3c8b800179b7ae5779e2999aa3375f74bd188d7d6703497f553f
erlang                      18.3                md5:7e4ff32f97c36fb3dab736f8d481830b
`

func createA1VersionManifest() error {
	if a1upgrade.Failure(a1upgrade.PreflightNoA1) {
		thPrintf("Not creating version manifest because of failure configuration")
		return nil
	}

	path := "/opt/delivery/version-manifest.txt"
	err := os.MkdirAll(filepath.Dir(path), os.ModePerm)
	if err != nil {
		return err
	}

	thPrintf("Creating %s\n", path)
	// Create sentinel file in this directory. This helps
	// with re-runs of the self-check mode to avoid the
	// sanity-check
	f, err := os.Create("/opt/delivery/.created-by-a2-self-test")
	if err != nil {
		thPrintf("error creating directory: %v\n", err)
		return err
	}
	err = f.Close()
	if err != nil {
		return err
	}

	a1version := fmt.Sprintf("%d.%d.%d", a1upgrade.A1RequiredMajorVersion, a1upgrade.A1RequiredMinorVersion, a1upgrade.A1RequiredPatchVersion)
	if a1upgrade.Failure(a1upgrade.PreflightA1TooOld) {
		a1version = "1.7.314"
	}

	a1ManifestContent := fmt.Sprintf(a1ManifestFmt, a1version)

	err = ioutil.WriteFile(path, []byte(a1ManifestContent), os.ModePerm)
	if err != nil {
		thPrintf("error creating A1 version manifest: %v\n", err)
		return err
	}

	return nil
}

func createA1EtcFiles() error {
	_, err := os.Stat("/etc/delivery/delivery-running.json")
	if os.IsNotExist(err) {
		thPrintf("No delivery-running.json found, writing dummy test configuration\n")
		config := fmt.Sprintf(a1config, deployment.LbFQDN())
		err = ioutil.WriteFile("/etc/delivery/delivery-running.json", []byte(config), 0600)
		if err != nil {
			return err
		}
	}

	_, err = os.Stat("/etc/delivery/delivery-secrets.json")
	if os.IsNotExist(err) {
		thPrintf("No delivery-secrets.json found, writing dummy test configuration\n")
		ds := a1upgrade.DeliverySecrets{}
		ds.Postgresql.SuperuserPassword = "test-harness-pg-password"
		err := writeAsJSONTo(ds, "/etc/delivery/delivery-secrets.json")
		if err != nil {
			return err
		}
	}

	_, err = os.Stat("/etc/opscode/chef-server-running.json")
	if os.IsNotExist(err) {
		thPrintf("No chef-server-running.json found, writing dummy test configuration\n")
		err = ioutil.WriteFile("/etc/opscode/chef-server-running.json", []byte(chefServerRunning), 0600)
		if err != nil {
			return err
		}
	}
	return nil
}

func writeTestCerts() error {
	a1SSLCert := fmt.Sprintf(a1SSLCertFmt, deployment.LbFQDN())
	_, err := os.Stat(a1SSLCert)
	if os.IsNotExist(err) {
		thPrintf("No self-signed SSL cert found, writing dummy cert\n")
		err = ioutil.WriteFile(a1SSLCert, []byte(sslCert), 0600)
		if err != nil {
			return err
		}
	}

	a1SSLKey := fmt.Sprintf(a1SSLKeyFmt, deployment.LbFQDN())
	_, err = os.Stat(a1SSLKey)
	if os.IsNotExist(err) {
		thPrintf("No self-signed SSL key found, writing dummy key\n")
		err = ioutil.WriteFile(a1SSLKey, []byte(sslKey), 0600)
		if err != nil {
			return err
		}
	}

	return nil
}

func writeAsJSONTo(a1Config interface{}, path string) error {
	// delivery-running.json and delivery-secrets.json are 0600 on ACC
	// delivery-running is actually owned by delivery:root but since we do
	// everything as root it should be fine to just make the files as root.
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		return err
	}
	a1JSON, err := json.MarshalIndent(a1Config, "", "    ")
	a1JSON = append(a1JSON, '\n')
	if err != nil {
		return err
	}
	_, err = f.Write(a1JSON)
	if err != nil {
		return err
	}
	return f.Close()
}

const a1config = `
{
  "delivery": {
    "fqdn": "%s",
    "ip_version": "ipv4",
    "delivery": {
      "git_repos": "/var/opt/delivery/delivery/git_repos",
      "ssl_certificates": {
      },
      "no_ssl_verification": [
      ],
      "ldap_hosts": [

      ],
      "ldap_port": 3269,
      "ldap_timeout": 5000,
      "ldap_base_dn": "OU=Employees,OU=Domain users,DC=examplecorp,DC=com",
      "ldap_bind_dn": "ldapbind",
      "ldap_bind_dn_password": "secret123",
      "ldap_encryption": "start_tls",
      "ldap_attr_login": "sAMAccountName",
      "ldap_attr_mail": "mail",
      "ldap_attr_full_name": "fullName",
      "proxy": {
        "host": null,
        "port": 0,
        "user": null,
        "password": null,
        "no_proxy": [
          "localhost",
          "127.0.0.1"
        ]
      }
    },
    "insights" : {
      "data_directory": "/var/opt/delivery/elasticsearch/data"
    },
    "postgresql": {
      "data_dir": "/var/opt/delivery/postgresql/9.2/data",
      "log_rotation": {
        "file_maxbytes": 104857600,
        "num_to_keep": 10
      },
      "username": "chef-pgsql",
      "superuser_enable": true,
      "superuser_username": "chef-pgsql",
      "vip": "127.0.0.1",
      "port": 5432,
      "listen_address": "0.0.0.0",
      "max_connections": 350,
      "md5_auth_cidr_addresses": [
        "127.0.0.1/32",
        "::1/128"
      ],
      "trust_auth_cidr_addresses": [

      ],
      "shmmax": 17179869184,
      "shmall": 4194304,
      "shared_buffers": "512MB",
      "work_mem": "8MB",
      "effective_cache_size": "128MB",
      "checkpoint_segments": 3,
      "checkpoint_timeout": "5min",
      "checkpoint_completion_target": 0.5,
      "checkpoint_warning": "30s"
    },
    "data_collector": {
      "token": "SI3JNdW4XIN3iNaNL2gWkSOjxowNQHW7r8NG9yV7fsI"
    },
    "elasticsearch": {
      "log_rotation": {
        "file_maxbytes": 104857600,
        "num_to_keep": 10
      },
      "urls": [
        "http://127.0.0.1:9200"
      ],
      "max_open_files": 65536,
      "max_map_count": 262144,
      "max_locked_memory": "unlimited",
      "memory": "1024m",
      "new_memory_size": "124m",
      "jvm_opts": [

      ],
      "enable_gc_log": false,
      "migration_client_timeout": 600,
      "nginx_proxy_url": "http://localhost:8080/elasticsearch/"
    },
    "nginx": {
      "dir": "/var/opt/delivery/nginx/",
      "access_log": {
        "buffer_size": "32k",
        "flush_time": "10s"
      },
      "ssl_port": 443,
      "enable_non_ssl": false,
      "non_ssl_port": 80,
      "ssl_protocols": "TLSv1.2",
      "ssl_ciphers": "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:AES256-GCM-SHA384:!aNULL:!eNULL:!EXPORT",
      "worker_processes": 4,
      "worker_connections": 10240,
      "worker_processor_method": "epoll",
      "multi_accept": "on",
      "sendfile": "on",
      "tcp_nopush": "on",
      "tcp_nodelay": "on",
      "gzip": "on",
      "gzip_http_version": "1.0",
      "gzip_comp_level": "2",
      "gzip_proxied": "any",
      "gzip_types": [
        "text/plain",
        "text/css",
        "text/xml",
        "text/javascript",
        "application/javascript",
        "application/json",
        "application/x-javascript",
        "application/xml",
        "application/xml+rss"
      ],
      "keepalive_timeout": 60,
      "keepalive_requests": 10000,
      "client_max_body_size": "250m",
      "client_body_buffer_size": "128k",
      "client_header_buffer_size": "1k",
      "large_client_header_buffers": {
        "number": 4,
        "size": "8k"
      }
    },
    "backup": {
      "access_key_id": null,
      "create_bucket": true,
      "bucket": null,
      "base_path": null,
      "chef_server_config": false,
      "config": {
        "enabled": true
      },
      "cron": {
        "enabled": false,
        "max_archives": 7,
        "max_snapshots": 7,
        "notation": "0 0 * * *"
      },
      "db": {
        "enabled": true
      },
      "delete": {
        "pattern": null,
        "max_archives": null,
        "max_snapshots": null
      },
      "digest": {
        "enabled": true,
        "length": 256,
        "value": null
      },
      "elasticsearch": {
        "access_key_id": null,
        "bucket": null,
        "base_path": null,
        "enabled": true,
        "location": "/var/opt/delivery/elasticsearch_backups",
        "lock_timeout": "600",
        "max_restore_bytes_per_sec": "40mb",
        "max_snapshot_bytes_per_sec": "40mb",
        "poll_interval": "5",
        "region": "us-east-1",
        "request_timeout": "300",
        "retry_limit": 3,
        "secret_access_key": null,
        "server_side_encryption": null,
        "snapshot_timeout": "600",
        "type": "fs",
        "wait_for_lock": true
      },
      "notifications": {
        "enabled": true
      },
      "force": false,
      "git": {
        "enabled": true
      },
      "license": {
        "enabled": true
      },
      "list": {
        "types": "all",
        "format": "text"
      },
      "location": "/var/opt/delivery/backups",
      "name": null,
      "quiet": false,
      "rabbit": {
        "enabled": false
      },
      "compliance_profiles": {
        "enabled": true
      },
      "region": null,
      "secret_access_key": null,
      "server_side_encryption": "AES256",
      "sse_customer_algorithm": null,
      "sse_customer_key": null,
      "sse_customer_key_md5": null,
      "ssekms_key_id": null,
      "staging_dir": null,
      "type": "fs",
      "retry_limit": 5,
      "wait": true
    },
    "reaper": {
      "mode": "delete",
      "retention_period_in_days": 14,
      "compliance_retention_period_in_days": null,
      "insights_retention_period_in_days": null,
      "archive_region": null,
      "archive_destination": "fs",
      "aws_access_key_id": null,
      "aws_secret_key": null,
      "s3_bucket_name": null,
      "server_side_encryption": null,
      "archive_filesystem_path": null,
      "evasive_maneuvers_enabled": true,
      "free_space_threshold_percent": 10,
      "repository": "fs-chef-automate-reaper"
    },
    "compliance_profiles": {
      "market_path": "/var/opt/delivery/compliance/market",
      "profiles_path": "/var/opt/delivery/compliance/profiles",
      "log_rotation": {
        "file_maxbytes": 10240000,
        "num_to_keep": 10
      }
    },
    "fips": {
      "enable": false
    },
    "notifications": {
      "rule_store_file": "/var/opt/delivery/notifications/rule_store",
      "log_rotation": {
        "file_maxbytes": 104857600,
        "num_to_keep": 10
      }
    }
  }
}
`

const chefServerRunning = `
{
	"private_chef": {
		"opscode-solr4": { "enable": true, "external": true, "external_url": "http://localhost:8080/elasticsearch" },
		"postgresql": { "enable": true, "external": true, "vip": "127.0.0.1", "port": 5432 },
		"bookshelf": { "enable": true, "storage_type": "sql" }
	}
}
`
