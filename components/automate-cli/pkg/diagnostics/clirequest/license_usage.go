package clirequest

import (
	"encoding/json"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/command"
)

type LicenseUsageNodeMetadata struct {
	Name         string `json:"name"`
	Organization string `json:"organization"`
}

type LicenseUsageNode struct {
	ID          string                   `json:"id"`
	LastSeen    string                   `json:"last_seen"`
	CheckinType string                   `json:"checkin_type"`
	Metadata    LicenseUsageNodeMetadata `json:"metadata"`
}

type LicenseUsageScanNode struct {
	ID        string `json:"id"`
	Name      string `json:"name"`
	ScanJobID string `json:"scan_job_id"`
}

type LicenseUsageResult struct {
	ScannedNodeCount int                    `json:"scanned_node_count"`
	ManagedNodeCount int                    `json:"managed_node_count"`
	ManagedNodes     []LicenseUsageNode     `json:"managed_nodes"`
	ScannedNodes     []LicenseUsageScanNode `json:"scanned_nodes"`
}

type LicenseUsageResp struct {
	Command   string             `json:"command"`
	Status    string             `json:"status"`
	ErrorCode int                `json:"error_code"`
	Result    LicenseUsageResult `json:"result"`
}

// LicenseUsage runs the 'license usage' command
func LicenseUsage() (*LicenseUsageResp, error) {
	cmd := Cmd("license", "usage", "--result-json", "/tmp/license_usage.json")
	out, err := cmd.Output()

	if err != nil {
		return nil, errors.Wrapf(err, "Could not run the license usage command:\nstdout:\n%s\n\nstderr:\n%s",
			out, command.StderrFromError(err))
	}

	resp := LicenseUsageResp{}

	// This is really clumsy. What I would prefer is a global option that if you set --test,
	// it gives you the status object as json

	jsonFile, err := os.Open("/tmp/license_usage.json")
	if err != nil {
		return nil, errors.Wrap(err, "Could not open /tmp/license_usage.json")
	}
	defer func() {
		_ = jsonFile.Close()
	}()

	byteValue, err := ioutil.ReadAll(jsonFile)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to read JSON output file.")
	}

	if err := json.Unmarshal(byteValue, &resp); err != nil {
		return nil, errors.Wrapf(err, "Failed to decode license usage response:\nstdout:%s", byteValue)
	}

	// Clean up the JSON file.
	err = os.Remove("/tmp/license_usage.json")
	if err != nil {
		return nil, errors.Wrap(err, "Failed to delete /tmp/license_usage.json")
	}

	return &resp, nil
}
