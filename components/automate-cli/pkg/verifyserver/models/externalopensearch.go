package models

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
