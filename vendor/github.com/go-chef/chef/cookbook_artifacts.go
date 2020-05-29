package chef

import "fmt"

// CBAService  is the service for interacting with chef server cookbook_artifacts endpoint
type CBAService struct {
	client *Client
}

// CBAGetResponse is returned from the chef-server for Get Requests to /cookbook_artifacts
type CBAGetResponse map[string]CBA
type CBA struct {
	Url         string       `json:"url,omitempty"`
	CBAVersions []CBAVersion `json:"versions,omitempty"`
}
type CBAVersion struct {
	Url        string `json:"url,omitempty"`
	Identifier string `json:"identifier,omitempty"`
}

// CBADetail represents the detail for a specific cookbook_artifact
type CBADetail struct {
	Version     string         `json:"version"`
	Name        string         `json:"name"`
	Identifier  string         `json:"identifier"`
	RootFiles   []CookbookItem `json:"root_files,omitempty"`
	Providers   []CookbookItem `json:"providers,omitempty"`
	Resources   []CookbookItem `json:"resources,omitempty"`
	Libraries   []CookbookItem `json:"libraries,omitempty"`
	Attributes  []CookbookItem `json:"attributes,omitempty"`
	Recipes     []CookbookItem `json:"recipes,omitempty"`
	Definitions []CookbookItem `json:"definitions,omitempty"`
	Files       []CookbookItem `json:"files,omitempty"`
	Templates   []CookbookItem `json:"templates,omitempty"`
	Frozen      bool           `json:"frozen?,omitempty"`
	ChefType    string         `json:"chef_type,omitempty"`
	Metadata    CBAMeta        `json:"metadata,omitempty"`
}

// CBAMeta represents the cookbook_artifacts metadata information
type CBAMeta struct {
	Name            string                 `json:"name,omitempty"`
	Version         string                 `json:"version,omitempty"`
	Description     string                 `json:"description,omitempty"`
	LongDescription string                 `json:"long_description,omitempty"`
	Maintainer      string                 `json:"maintainer,omitempty"`
	MaintainerEmail string                 `json:"maintainer_email,omitempty"`
	License         string                 `json:"license,omitempty"`
	Platforms       map[string]string      `json:"platforms,omitempty"`
	Depends         map[string]string      `json:"dependencies,omitempty"`
	Reccomends      map[string]string      `json:"recommendations,omitempty"`
	Suggests        map[string]string      `json:"suggestions,omitempty"`
	Conflicts       map[string]string      `json:"conflicting,omitempty"`
	Provides        map[string]string      `json:"providing,omitempty"`
	Replaces        map[string]string      `json:"replacing,omitempty"`
	Attributes      map[string]interface{} `json:"attributes,omitempty"` // this has a format as well that could be typed, but blargh https://github.com/lob/chef/blob/master/cookbooks/apache2/metadata.json
	Groupings       map[string]interface{} `json:"groupings,omitempty"`  // never actually seen this used.. looks like it should be map[string]map[string]string, but not sure http://docs.opscode.com/essentials_cookbook_metadata.html
	Recipes         map[string]string      `json:"recipes,omitempty"`
	SourceURL       string                 `json:"source_url,omitempty"`
	IssuesURL       string                 `json:"issues_url,omitempty"`
	Privacy         bool                   `json:"privacy,omitempty"`
	ChefVersions    [][]string             `json:"chef_versions,omitempty"`
	OhaiVersions    []string               `json:"ohai_versions,omitempty"`
	Gems            []string               `json:"gems,omitempty"`
}

// List lists the Cookbook_Artifacts in the Chef server.
//  GET /cookbook_artifacts
func (c *CBAService) List() (data CBAGetResponse, err error) {
	err = c.client.magicRequestDecoder("GET", "cookbook_artifacts", nil, &data)
	return
}

// Get returns details for a specific cookbook artifact
//  GET /cookbook_artifacts/name
func (c *CBAService) Get(name string) (data CBAGetResponse, err error) {
	path := fmt.Sprintf("cookbook_artifacts/%s", name)
	err = c.client.magicRequestDecoder("GET", path, nil, &data)
	return
}

// GetVersion fetches a specific version of a cookbook_artifact from the server api
//  GET /cookbook_artifact/foo/1ef062de1bc4cb14e4a78fb739e104eb9508473e
func (c *CBAService) GetVersion(name, id string) (data CBADetail, err error) {
	url := fmt.Sprintf("cookbook_artifacts/%s/%s", name, id)
	err = c.client.magicRequestDecoder("GET", url, nil, &data)
	return
}
