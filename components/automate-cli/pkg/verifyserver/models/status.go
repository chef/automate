package models

type ServiceDetails struct {
	ServiceName string `json:"service_name"`
	Status      string `json:"status"`
	Version     string `json:"version"`
}

type StatusDetails struct {
	Status     string            `json:"status"`
	Services   *[]ServiceDetails `json:"services"`
	CliVersion string            `json:"cli_version"`
	Error      string            `json:"error"`
}

type StatusApiResponse struct {
	Status string        `json:"status"`
	Result StatusDetails `json:"result"`
}
