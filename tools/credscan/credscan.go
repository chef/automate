package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

type pattern struct {
	regex       string
	description string
}

type config struct {
	// A file will be listed as a violation if it matches the
	// given filenameInclude patterns.
	filenameInclude []pattern
	// Files that match the filenameExclude patterns are excluded
	// from both filename matches and content matches.
	filenameExclude []pattern

	// A file will be listed as a violation if the /content/
	// matches the content patterns.
	contentInclude []pattern
	// A content violation above will be excluded if it matches
	// any of the given patterns.
	contentExclude []pattern
}

// Credential scanning configuration for the Automate 2 repository. If
// we find this useful we can move it out into a configuration file.
var a2Config = config{
	filenameInclude: []pattern{
		{
			description: "PEM-encoded file, possible private key",
			regex:       `\.pem`,
		},
		{
			description: ".key file, possible private key file",
			regex:       `\.key`,
		},
		{
			description: "JSON web token (A2 licenses)",
			regex:       `\.jwt`,
		},
		{
			description: "A2 licenses",
			regex:       `\.license$`,
		},
	},
	filenameExclude: []pattern{
		{regex: `api/config/deployment/automate_config_test.go`},
		{
			description: "development folder credentials",
			regex:       `^dev/certs/.*\.key`,
		},
		{
			description: "habitat certificate templates",
			regex:       `^components/[^/]*/habitat/config/service.key`,
		},
		{
			description: "ignore ourselves",
			regex:       `tools/credscan/credscan.go`,
		},
		{
			description: "corrupt a2 license for testing",
			regex:       `components/license-control-service/testdata/a2-test_corrupt.license`,
		},
		// // TODO(ssd) 2019-01-22: The following files are files that need to be checked.
		{regex: `components/automate-deployment/pkg/persistence/boltdb/internal/v1/testdata/boltdb-20180226095149.db`},
		{regex: `components/automate-chef-io/content/docs/configuration.md`},
		{regex: `components/automate-deployment/cmd/chef-server-ctl/secrets.go`},
		{regex: `components/automate-deployment/pkg/a1stub/test_harness.go`},
		{regex: `components/automate-deployment/pkg/a1upgrade/a1commands_test.go`},
		{regex: `components/automate-deployment/pkg/assets/data/a1stub_certs/ChefAutomateUpgradeFromv1SelfTest.key`},
		{regex: `components/automate-deployment/testdata/hab_responses/all_up.json`},
		{regex: `components/automate-deployment/tools/upgrade-test-scaffold/upgrade-test-scaffold.go`},
		{regex: `components/automate-ui/src/app/pages/\+compliance/\+credentials/components/credentials-form.html`},
		{regex: `components/backup-gateway/habitat/config/private.key`},
		{regex: `components/compliance-service/api/tests/containers/key.pem`},
		{regex: `components/compliance-service/docs/elastic/api_compliance.rst`},
		{regex: `components/compliance-service/generator/targets/client_containers/vagrant`},
		{regex: `components/compliance-service/generator/vagrant.key`},
		{regex: `dev/config.toml`},
		{regex: `lib/grpc/secureconn/testdata/.*\.key`},

		// Test data from workflow
		{regex: `components/automate-workflow-server/apps/delivery/test/unit/data/certificates/expired.key`},
		{regex: `components/automate-workflow-server/apps/delivery/test/unit/data/certificates/untrusted.key`},
		{regex: `components/automate-workflow-server/apps/jobs/test/unit/data/fips-private-key`},
		{regex: `components/automate-workflow-server/apps/jobs/test/unit/jobs_test_utils.erl`},
		{regex: `components/automate-workflow-server/etc/builder_key`},
		{regex: `components/automate-workflow-server/apps/delivery/test/unit/data/certificates/expired.key`},
		{regex: `components/automate-workflow-server/apps/delivery/test/unit/data/certificates/untrusted.key`},

		// Habitat configuration templates, no acutal creds
		{regex: `components/automate-dex/habitat/config/ldap-ca.pem`},
		{regex: `components/automate-dex/habitat/config/saml-ca.pem`},
		{regex: `components/automate-postgresql/habitat/config/server.key`},
	},
	contentInclude: []pattern{
		{
			description: "slack hooks",
			regex:       `https://hooks.slack.com`,
		},
		{
			description: "private keys",
			regex:       `BEGIN.*PRIVATE`,
		},
		// The following regular expressions were taken from git-secrets
		//
		// https://github.com/awslabs/git-secrets/blob/79a73d8ade413f4e15f24f935bc903d53d3fbaf2/git-secrets
		//
		// which contained the following copyright notice:
		//
		// # Copyright 2010-2013 Amazon.com, Inc. or its affiliates. All Rights Reserved.
		// #
		// # Licensed under the Apache License, Version 2.0 (the "License").
		// # You may not use this file except in compliance with the License.
		// # A copy of the License is located at
		// #
		// # http://aws.amazon.com/apache2.0
		// #
		// # or in the "license" file accompanying this file. This file is distributed
		// # on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
		// # express or implied. See the License for the specific language governing
		// # permissions and limitations under the License.
		//
		{
			description: "aws access key ids",
			regex:       `(A3T[A-Z0-9]|AKIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA)[A-Z0-9]{16}`,
		},
		{
			description: "variations of aws_account_id",
			regex:       `("|')?(AWS|aws|Aws)?_?(ACCOUNT|account|Account)_?(ID|id|Id)?("|')?\s*(:|=>|=)\s*("|')?[0-9]{4}\-?[0-9]{4}\-?[0-9]{4}("|')?`,
		},
		{
			description: "variations of aws_secret_access_key",
			regex:       `("|')?(AWS|aws|Aws)?_?(SECRET|secret|Secret)?_?(ACCESS|access|Access)?_?(KEY|key|Key)("|')?\s*(:|=>|=)\s*("|')?[A-Za-z0-9/\+=]{40}("|')?`,
		},
	},
	contentExclude: []pattern{
		{
			description: "fake account ID",
			regex:       "999999999999",
		},
		{
			description: "fake AWS account ID",
			regex:       "AKIAIOSFODNN7EXAMPLE",
		},
		{
			description: "fake AWS secret key",
			regex:       "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
		},
		{
			description: "OpenJDK PGP key",
			regex:       "DA1A4A13543B466853BAF164EB9B1D8886F44E2A",
		},
		{
			description: "fake slack webhook 1",
			regex:       "https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE",
		},
		{
			description: "fake slack webhook 2",
			regex:       "https://hooks.slack.com/services/T00000000",
		},
	},
}

func main() {
	for _, cmd := range []string{"git", "grep", "bash"} {
		_, err := exec.LookPath(cmd)
		if err != nil {
			fatalf("credscan requires %q\n", cmd)
		}
	}

	cmd := exec.Command("git", "rev-parse", "--git-dir")
	err := cmd.Run()
	if err != nil {
		fatalf("credscan should be run from inside the git repository\n")
	}

	// NOTE(ssd) 2019-01-22: Why shell out to git grep?
	//
	// git-grep is much faster than a trivial go
	// implementation. git-grep searches in parallel and is able
	// to read from the git object store directly. While I am sure
	// we could create something in Go that is as fast, let's wait
	// until we need more flexibility.
	//
	outbuf := &bytes.Buffer{}
	gitGrepCmd := fmt.Sprintf("git grep -a -E \"%s\" | grep -v -E \"%s\"",
		toGrepString(a2Config.contentInclude),
		toGrepString(append(a2Config.contentExclude, a2Config.filenameExclude...)))
	cmd = exec.Command("bash", "-c", gitGrepCmd)
	cmd.Stdout = outbuf
	cmd.Stderr = outbuf
	cmd.Run() // nolint: errcheck

	// git ls-files for filenames
	filesCmd := fmt.Sprintf("git ls-files | grep -E \"%s\" | grep -v -E \"%s\"",
		toGrepString(a2Config.filenameInclude),
		toGrepString(a2Config.filenameExclude))
	cmd = exec.Command("bash", "-c", filesCmd)
	cmd.Stdout = outbuf
	cmd.Stderr = outbuf
	cmd.Run() // nolint: errcheck

	fmt.Print(outbuf.String())
	if outbuf.Len() > 0 {
		fmt.Fprintln(os.Stderr, "Possible credentials found. See above output for details.")
		os.Exit(1)
	}
}

// TODO(ssd) 2019-01-23: git-grep and grep both accept patterns via a
// file with the `--file` option.  We should move to this if our
// pattern list grows too large to pass on the command line.  That
// would also avoid escaping.
func toGrepString(patterns []pattern) string {
	singleQuery := concatRegexString(patterns)
	singleQuery = strings.Replace(singleQuery, `\`, `\\`, -1)
	singleQuery = strings.Replace(singleQuery, `"`, `\"`, -1)
	return singleQuery
}

func concatRegexString(patterns []pattern) string {
	strList := make([]string, 0, len(patterns))
	for _, p := range patterns {
		strList = append(strList, p.regex)
	}
	return strings.Join(strList, "|")
}

func fatalf(msg string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, msg, args...)
	os.Exit(1)
}
