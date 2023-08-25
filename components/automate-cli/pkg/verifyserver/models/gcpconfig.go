package models

type GCPCloudStorageConfigRequest struct {
	Endpoint                 string             `json:"endpoint"`
	BucketName               string             `json:"bucket_name"`
	GoogleServiceAccountFile string             `json:"google_service_account_file"`
	GcpServiceAccount        *GcpServiceAccount `json:"gcp_service_account"`
}
type GCPCloudStorageResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
