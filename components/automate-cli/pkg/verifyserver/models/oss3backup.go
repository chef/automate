package models

type S3BackupDetails struct {
	Endpoint   string `json:"endpoint" validate:"required"`
	Username   string `json:"username" validate:"required"`
	Password   string `json:"password" validate:"required"`
	S3Bucket   string `json:"s3_bucket" validate:"required"`
	S3BasePath string `json:"s3_basepath" validate:"required"`
	AccessKey  string `json:"aws_access_key" validate:"required"`
	SecretKey  string `json:"aws_secret_key" validate:"required"`
	AWSRegion  string `json:"aws_region" validate:"required"`
	AWSRoleArn string `json:"aws_role_arn" validate:"required"`
}

type S3BackupManagedResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
