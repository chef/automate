package parser

import (
	"encoding/json"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/pkg/errors"
)

// TODO(ssd) 2018-02-07: Bad name, not a manifest. Just used to
// jump-start the parsing into one of the versioned structs.
type versionedManifest struct {
	// TODO(ssd) 2018-02-07: Why is this a string in the JSON?
	SchemaVersion string `json:"schema_version"`
}

// A V1Manifest is a Manifest constructed from an S3 manifest with
// schema_version = 1.
type v1Manifest struct {
	SchemaVersion string          `json:"schema_version"`
	SHA           string          `json:"git_sha"`
	BuildT        string          `json:"build"`
	Packages      []habpkg.HabPkg `json:"packages"`
	HabPackages   []habpkg.HabPkg `json:"hab"`
}

// ManifestFromBytes parses the passed []byte's assuming it is either an A2
// manifest or a versioned S3 manifest. It will return an error if it was
// unable to parse the manifest.
func ManifestFromBytes(body []byte) (*manifest.A2, error) {
	// Try an A2 manifest. If it doesn't have a BuildSHA or returns an error
	// then try a versioned manifest.
	a2, _ := parseA2Manifest(body)
	if a2 != nil && a2.BuildSHA != "" {
		return a2, nil
	}

	versionedManifest := &versionedManifest{}
	err := json.Unmarshal(body, versionedManifest)
	if err != nil {
		return nil, manifest.NewInvalidSchemaError(err)
	}

	ver := versionedManifest.SchemaVersion
	switch ver {
	case "1":
		return parseV1Manifest(body)
	default:
		return nil, manifest.NewInvalidSchemaError(errors.Errorf("schema version unknown: %s", ver))
	}
}

func parseV1Manifest(body []byte) (*manifest.A2, error) {
	v1 := &v1Manifest{}
	err := json.Unmarshal(body, v1)
	if err != nil {
		return nil, manifest.NewCannotParseError(err)
	}

	m := &manifest.A2{}
	m.Build = v1.BuildT
	m.BuildSHA = v1.SHA
	m.Packages = append(v1.Packages, v1.HabPackages...)

	return m, nil
}

func parseA2Manifest(body []byte) (*manifest.A2, error) {
	a2 := &manifest.A2{}
	err := json.Unmarshal(body, a2)
	if err != nil {
		return nil, manifest.NewCannotParseError(err)
	}

	return a2, nil
}
