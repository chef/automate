package models

type SshUserChecksRequest struct {
	Ip            string `json:"ip"`
	User_Name     string `json:"user_name"`
	Port          string `json:"ssh_port"`
	Private_Key   string `json:"private_key"`
	Sudo_Password string `json:"sudo_password"`
}

type SshUserChecksResponse struct {
	Passed bool   `json:"passed"`
	Checks []Checks `json:"checks"`
}
