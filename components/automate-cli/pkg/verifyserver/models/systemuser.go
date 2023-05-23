package models

type SystemUserServiceCheck struct {
    Title         string `json:"title"`
    Passed        bool   `json:"passed"`
    SuccessMsg    string `json:"success_msg"`
    ErrorMsg      string `json:"error_msg"`
    ResolutionMsg string `json:"resolution_msg"`
}

type SystemUserResponse struct {
    Passed bool           `json:"passed"`
    Checks []SystemUserServiceCheck `json:"checks"`
}
