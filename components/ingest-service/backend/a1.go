package backend

import "time"

// InsightsRun - converge data pulled from an A1 insights index.
type InsightsRun struct {
	RunID                string     `json:"run_id"`
	StartTime            time.Time  `json:"start_time"`
	EndTime              time.Time  `json:"end_time"`
	TotalResourceCount   int        `json:"total_resource_count"`
	UpdatedResourceCount int        `json:"updated_resource_count"`
	Node                 string     `json:"node"` // escaped json
	Resources            []Resource `json:"resources"`
}

type InsightsRunNodePayLoadData struct {
	RunID string `json:"run_id"`
	Node  string `json:"node"` // escaped json
}
