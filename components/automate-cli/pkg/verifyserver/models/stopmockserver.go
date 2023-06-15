package models

// StopMockServerRequestBody contains the configuration for starting a mock server.
type StopMockServerRequestBody struct {
	Port     int
	Protocol string
}

type StopMockServerResponse struct {
	Status string `json:"status"`
	Result string `json:"result"`
}
