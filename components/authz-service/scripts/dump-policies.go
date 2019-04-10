package main

import (
	"encoding/json"
	"fmt"
	"os"

	storage "github.com/chef/automate/components/authz-service/storage/v1"
)

// This is what the JSON output of a policy looks like here -- note that we're
// not outputting everything; some fields are suppressed (CreatedAt, Effect,
// Version, ...)
type policy struct {
	Subjects  []string `json:"subjects"`
	Resource  string   `json:"resource"`
	Action    string   `json:"action"`
	Deletable bool     `json:"deletable"`
}

func main() {
	pols, err := storage.DefaultPolicies()
	if err != nil {
		fail(err)
	}
	out := map[string]policy{}
	for id, pol := range pols {
		out[id] = policy{
			Subjects:  pol.Subjects,
			Resource:  pol.Resource,
			Action:    pol.Action,
			Deletable: !storage.IsNonDeletablePolicy(id),
		}
	}
	err = json.NewEncoder(os.Stdout).Encode(out)
	if err != nil {
		fail(err)
	}
}

func fail(err error) {
	fmt.Fprintf(os.Stderr, err.Error())
	os.Exit(1)
}
