package reporting

type NodeControlSummary struct {
	Total  int `json:"total"`
	Passed struct {
		Total int `json:"total"`
	} `json:"passed"`
	Skipped struct {
		Total int `json:"total"`
	} `json:"skipped"`
	Failed struct {
		Total    int `json:"total"`
		Minor    int `json:"minor"`
		Major    int `json:"major"`
		Critical int `json:"critical"`
	} `json:"failed"`
}

type ProfileMin struct {
	Name    string `json:"name"`
	Title   string `json:"title"`
	ID      string `json:"id"`
	Version string `json:"version"`
	Status  string `json:"status,omitempty"`
}
