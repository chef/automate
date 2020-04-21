package chef

type LicenseService struct {
	client *Client
}

// License represents the body of the returned information.
type License struct {
	LimitExceeded bool   `json:"limit_exceeded"`
	NodeLicense   int    `json:"node_license"`
	NodeCount     int    `json:"node_count"`
	UpgradeUrl    string `json:"Upgrade_url"`
}

// License gets license information.
//
// https://docs.chef.io/api_chef_server/#license
func (e *LicenseService) Get() (data License, err error) {
	err = e.client.magicRequestDecoder("GET", "license", nil, &data)
	return
}
