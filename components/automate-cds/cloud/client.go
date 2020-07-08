package cloud

import "github.com/chef/automate/components/automate-cds/creds"

type Client struct {
	items []ContentItem
}

// NewClient - create a new backend client
func NewClient() Client {
	return Client{
		items: getItem(),
	}
}

// GetContentItems - Returns a list of CDS content items
func (client *Client) GetContentItems(credentials creds.Credentials) ([]ContentItem, error) {
	return client.items, nil
}

// GetContentItem - Get a content item
func (client *Client) GetContentItem(id string, credentials creds.Credentials) (ContentItem, bool, error) {
	for _, item := range client.items {
		if item.ID == id {
			return item, true, nil
		}
	}

	return ContentItem{}, false, nil
}

// VerifyCredentials - return true if the token is valid and false otherwise.
func (client *Client) VerifyCredentials(credentials creds.Credentials) (bool, error) {
	isNotValid :=
		"wrong" == credentials.ClientID ||
			"wrong" == credentials.ClientSecret ||
			"wrong" == credentials.TenantSpecificURL

	return !isNotValid, nil
}

func getItem() []ContentItem {
	return []ContentItem{
		{
			ID:             "f354577e-2ea5-4c96-86ed-78a8ba68e0f3",
			Name:           "DevSec Apache Baseline",
			Description:    "Test-suite for best-practice apache hardening",
			Type:           "profile",
			Version:        "2.1.2",
			Platforms:      []string{"unix"},
			CanBeInstalled: true,
			Filename:       "apache-baseline-2.2.2.tar.gz",
			DownloadURL:    "https://github.com/dev-sec/apache-baseline/archive/master.tar.gz",
		},
		{
			ID:             "1f9e6a1e-3381-487f-98ef-9966c65ff92a",
			Name:           "Audit Benchmark level 1",
			Description:    "Test suite for best practice Linux OS hardening",
			Type:           "cookbook",
			Version:        "9.2.1",
			Platforms:      []string{"Redhat", "Centos"},
			CanBeInstalled: false,
			Filename:       "audit-benchmark-level-1.tar.gz",
			DownloadURL:    "https://github.com/dev-sec/apache-baseline/archive/master.tar.gz",
		},
		{
			ID:             "736f5724-b384-4d74-b8cb-dedd9f1ecd54",
			Name:           "Compliance Effortless Package",
			Description:    "This is a hart file that enables effortless compliance on your fleet.",
			Type:           "package",
			Version:        "2.3.2",
			Platforms:      []string{"linux"},
			CanBeInstalled: false,
			Filename:       "chef-compliance-effortless-1.1.1-20200626161151-x86_64-linux.hart",
			DownloadURL:    "https://bldr.habitat.sh/v1/depot/pkgs/effortless/audit-baseline/0.1.0/20191217125442/download",
		},
		{
			ID:             "606ef343-b86d-40e8-b633-711e1469c49e",
			Name:           "DevSec Linux Security Baseline",
			Description:    "Test suite for best practice Linux OS hardening",
			Type:           "profile",
			Version:        "2.4.5",
			Platforms:      []string{"linux"},
			CanBeInstalled: true,
			Filename:       "linux-baseline-2.2.2.tar.gz",
			DownloadURL:    "https://github.com/dev-sec/linux-baseline/archive/master.tar.gz",
		},
		{
			ID:             "89ac5083-d852-4227-aa5d-0b1678c52556",
			Name:           "DevSec Windows Security Baseline",
			Description:    "An InSpec Compliance Profile that covers CIS Microsoft Windows Server 2012R2, 2016 RTM (Release 1607) Benchmark Level 1 and 2 and additional controls from MS technet.",
			Type:           "profile",
			Version:        "2.1.4",
			Platforms:      []string{"windows"},
			CanBeInstalled: true,
			Filename:       "windows-baseline-2.2.2.tar.gz",
			DownloadURL:    "https://github.com/dev-sec/windows-baseline/archive/master.tar.gz",
		},
	}
}

// ContentItem - description of one content item
type ContentItem struct {
	ID             string
	Name           string
	Description    string
	Type           string
	Version        string
	Platforms      []string
	CanBeInstalled bool
	Filename       string
	DownloadURL    string
}
