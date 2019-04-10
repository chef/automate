package clirequest

import (
	"encoding/json"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/command"
)

type NodeInventoryNode struct {
	Name         string `json:"name"`
	Organization string `json:"organization"`
	Status       string `json:"status"`
	Checkin      string `json:"checkin"`
}

type NodeInventoryResp struct {
	Command          string              `json:"command"`
	Status           string              `json:"status"`
	ErrorDescription string              `json:"error_description"`
	Nodes            []NodeInventoryNode `json:"nodes"`
	NodeCount        int                 `json:"node_count"`
}

// NodeInventory runs the 'chef-automate node-inventory' command
func NodeInventory() (*NodeInventoryResp, error) {
	cmd := Cmd("node-inventory")
	out, err := cmd.Output()
	if err != nil {
		return nil, errors.Wrapf(err, "Could not run the node-inventory command:\nstdout:\n%s\n\nstderr:\n%s",
			out, command.StderrFromError(err))
	}

	resp := NodeInventoryResp{}
	if err := json.Unmarshal(out, &resp); err != nil {
		return nil, errors.Wrapf(err, "Failed to decode node-inventory response:\nstdout:%s", out)
	}

	return &resp, nil
}
