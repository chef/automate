package models


type S3ConfigRequest struct {
	Endpoint   string `json:"endpoint"`
	BucketName string `json:"bucket_name"`
	BasePath   string `json:"base_path"`
	AccessKey  string `json:"access_key"`
	SecretKey  string `json:"secret_key"`
	Region     string `json:"region"`
}
type S3ConfigResponse struct {
	Passed bool             `json:"passed"`
	Checks []Checks `json:"checks"`
}
