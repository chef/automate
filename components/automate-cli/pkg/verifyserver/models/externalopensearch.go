package models

type ExternalOSRequest struct {
	OSDomainName   string `json:"opensearch_domain_name"`
	OSDomainURL    string `json:"opensearch_domain_url"`
	OSUsername     string `json:"opensearch_username"`
	OSUserPassword string `json:"opensearch_user_password"`
	OSCert         string `json:"opensearch_root_cert"`
}

type ExternalOpensearchResponse struct {
	Passed bool                      `json:"passed"`
	Checks []ExternalOpensearchCheck `json:"checks"`
}
type ExternalOpensearchCheck struct {
	Title         string `json:"title"`
	Passed        bool   `json:"passed"`
	Status        string `json:"status"`
	SuccessMsg    string `json:"success_msg"`
	ErrorMsg      string `json:"error_msg"`
	ResolutionMsg string `json:"resolution_msg"`
	DebugMsg      string `json:"debug_msg"`
}
