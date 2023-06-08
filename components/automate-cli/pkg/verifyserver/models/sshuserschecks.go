package models

type SshUserChecksRequest struct {
	Ip           string `json:"ip"`
	UserName     string `json:"user_name"`
	Port         string `json:"ssh_port"`
	PrivateKey   string `json:"private_key"`
	SudoPassword string `json:"sudo_password"`
}

type SshUserChecksResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
