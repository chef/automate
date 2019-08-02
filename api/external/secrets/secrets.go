package secrets

import (
	"encoding/json"

	"github.com/chef/automate/lib/errorutils"
	logs "github.com/sirupsen/logrus"
)

const (
	requiredNameError                      = "Invalid secret, 'name' is a required parameter"
	requiredSSHUsernameError               = "Invalid data content for secret type 'ssh'. A 'username' field is required"
	requiredSSHPasswordOrKeyError          = "Invalid data content for secret type 'ssh'. A 'password' or 'key' field is required"
	requiredSSHExclusivePasswordOrKeyError = "Invalid data content for secret type 'ssh'. 'password' and 'key' fields are mutually exclusive"
	requiredServiceNowUsernameError        = "Invalid data content for secret type 'service_now'. A 'username' field is required"
	requiredServiceNowPasswordError        = "Invalid data content for secret type 'service_now'. A 'password' field is required"
	requiredWinrmUsernameError             = "Invalid data content for secret type 'winrm'. A 'username' field is required"
	requiredWinrmPasswordError             = "Invalid data content for secret type 'winrm'. A 'password' field is required"
	requiredSudoPasswordOrOptionError      = "Invalid data content for secret type 'sudo'. A 'password' or 'options' field is required"
	requiredAwsAccessKeyError              = "Invalid data content for secret type 'aws'. AWS_ACCESS_KEY_ID not provided"
	requiredAwsSecretAccessKeyError        = "Invalid data content for secret type 'aws'. AWS_SECRET_ACCESS_KEY not provided"
	requiredAzureClientIDError             = "Invalid data content for secret type 'azure'. AZURE_CLIENT_ID not provided"
	requiredAzureClientSecretError         = "Invalid data content for secret type 'azure'. AZURE_CLIENT_SECRET not provided"
	requiredAzureTenantIDError             = "Invalid data content for secret type 'azure'. AZURE_TENANT_ID not provided"
	requiredGcpCredentialsJsonError        = "Invalid data content for secret type 'gcp'. GOOGLE_CREDENTIALS_JSON not provided"
)

type GcpCredential struct {
	Type                    string `json:"type"`
	ProjectID               string `json:"project_id"`
	ClientID                string `json:"client_id"`
	PrivateKeyID            string `json:"private_key_id"`
	PrivateKey              string `json:"private_key"`
	ClientEmail             string `json:"client_email"`
	AuthUri                 string `json:"auth_uri"`
	TokenUri                string `json:"token_uri"`
	AuthProviderX509CertUrl string `json:"auth_provider_x509_cert_url"`
	ClientX509CertUrl       string `json:"client_x509_cert_url"`
}

// Validate validates a Secret and returns the first validation error encountered.
func (s *Secret) Validate() error {
	errors := make([]*error, 0)
	errors = requiredField(s.Name, requiredNameError, errors)

	kvMap := make(map[string]string, len(s.Data))
	for _, kv := range s.Data {
		kvMap[kv.Key] = kv.Value
	}

	switch s.Type {
	case "ssh":
		errors = requiredField(kvMap["username"], requiredSSHUsernameError, errors)
		errors = requiredChoice([]string{kvMap["password"], kvMap["key"]}, requiredSSHPasswordOrKeyError, errors)
		errors = requiredExclusiveChoice([]string{kvMap["password"], kvMap["key"]}, requiredSSHExclusivePasswordOrKeyError, errors)
	case "winrm":
		errors = requiredField(kvMap["username"], requiredWinrmUsernameError, errors)
		errors = requiredField(kvMap["password"], requiredWinrmPasswordError, errors)
	case "sudo":
		errors = requiredChoice([]string{kvMap["password"], kvMap["options"]}, requiredSudoPasswordOrOptionError, errors)
	case "aws":
		if kvMap["ARN_ROLE"] == "" {
			errors = requiredField(kvMap["AWS_ACCESS_KEY_ID"], requiredAwsAccessKeyError, errors)
			errors = requiredField(kvMap["AWS_SECRET_ACCESS_KEY"], requiredAwsSecretAccessKeyError, errors)
		}
	case "azure":
		errors = requiredField(kvMap["AZURE_CLIENT_ID"], requiredAzureClientIDError, errors)
		errors = requiredField(kvMap["AZURE_CLIENT_SECRET"], requiredAzureClientSecretError, errors)
		errors = requiredField(kvMap["AZURE_TENANT_ID"], requiredAzureTenantIDError, errors)
	case "gcp":
		errors = requiredField(kvMap["GOOGLE_CREDENTIALS_JSON"], requiredGcpCredentialsJsonError, errors)
		_, err := UnmarshalGcpServiceAcc(kvMap["GOOGLE_CREDENTIALS_JSON"])
		if err != nil {
			errors = append(errors, &err)
		}
	case "service_now":
		errors = requiredField(kvMap["username"], requiredServiceNowUsernameError, errors)
		errors = requiredField(kvMap["password"], requiredServiceNowPasswordError, errors)
	}

	// Eventually I'd like to switch our error handling to be handle an aggregation of errors
	// for now we only support one failrue so I'm returning the first one we encounter
	if len(errors) > 0 {
		return *errors[0]
	}

	return nil
}

// Merge combines the existing secret with fields from the new secret. Fields they have in
// common will replaced with the fields from new. Fields found in the Data attribute will not be
// removed from old if they are not found in new.
func (s *Secret) Merge(newSecret *Secret) {
	if newSecret.Name != "" {
		s.Name = newSecret.Name
	}
	if newSecret.Type != "" {
		s.Type = newSecret.Type
	}

	// Tags updating requires all needed tags to be sent in
	if len(newSecret.Tags) > 0 {
		s.Tags = newSecret.Tags
	}

	oldData := kvsToMap(s.Data)
	newData := kvsToMap(newSecret.Data)

	if newData["username"] != "" {
		oldData["username"] = newData["username"]
	}
	if newData["password"] != "" {
		oldData["password"] = newData["password"]
		if s.Type == "ssh" {
			delete(oldData, "key")
		}
	}
	if newData["key"] != "" {
		oldData["key"] = newData["key"]
		if s.Type == "ssh" {
			delete(oldData, "password")
		}
	}
	if newData["options"] != "" && s.Type == "sudo" {
		oldData["options"] = newData["options"]
	}
	if s.Type == "aws" || s.Type == "azure" || s.Type == "gcp" {
		// just go ahead and replace the old data with the new
		// stuff for the keys
		s.Data = mapToKvs(newData)
	} else {
		s.Data = mapToKvs(oldData)
	}

	logs.WithFields(logs.Fields{
		"Name": s.Name,
		"Type": s.Type,
		"Tags": s.Tags,
	}).Debug("existing secret merged with new secret")
}

func kvsToMap(kvs []*Kv) map[string]string {
	m := make(map[string]string)
	for _, kv := range kvs {
		m[kv.Key] = kv.Value
	}
	return m
}

func mapToKvs(m map[string]string) []*Kv {
	kvs := make([]*Kv, 0)
	for key, value := range m {
		kvs = append(kvs, &Kv{
			Key:   key,
			Value: value,
		})
	}
	return kvs
}

// requiredField adds to the error collection if the value is empty. A non empty value
// will not add an error and be considered valid.
func requiredField(value string, message string, errorList []*error) []*error {
	if value == "" {
		newErrors := append(make([]*error, 0), errorList...)
		err := errorutils.ProcessInvalid(nil, message)
		return append(newErrors, &err)
	}

	return errorList
}

// requiredChoice adds to the error collection if all of the values are empty. A non empty value
// will not add an error and be considered valid.
func requiredChoice(values []string, message string, errorList []*error) []*error {
	for _, value := range values {
		if value != "" {
			return errorList
		}
	}

	newErrors := append(make([]*error, 0), errorList...)
	err := errorutils.ProcessInvalid(nil, message)
	return append(newErrors, &err)
}

// requiredExclusiveChoice adds to the error collection if more than one of the
// values is present.
func requiredExclusiveChoice(values []string, message string, errorList []*error) []*error {
	presentValues := 0
	for _, value := range values {
		if value != "" {
			presentValues += 1
		}
	}

	if presentValues == 1 {
		return errorList
	}

	newErrors := append(make([]*error, 0), errorList...)
	err := errorutils.ProcessInvalid(nil, message)
	return append(newErrors, &err)
}

// UnmarshalGcpServiceAcc receives the GCP credential as a json string, unmarshals it
// and verifies that it's a service account, recommended by Google for API integrations
func UnmarshalGcpServiceAcc(gcpJSONCred string) (gcpCred *GcpCredential, err error) {
	err = json.Unmarshal([]byte(gcpJSONCred), &gcpCred)
	if err != nil {
		return nil, errorutils.ProcessInvalid(err, "Unable to unmarshal Google Credentials JSON")
	}
	if gcpCred.Type != "service_account" {
		return nil, errorutils.ProcessInvalid(nil, "Only 'service_account' type is supported for GOOGLE_CREDENTIALS_JSON")
	}
	return gcpCred, nil
}
