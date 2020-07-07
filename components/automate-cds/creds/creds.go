package creds

type Secrets struct {
	stored Credentials
}

type Credentials struct {
	ClientID          string
	ClientSecret      string
	TenantSpecificURL string
}

func CreateCredentials(clientID string, clientSecret string, tenantSpecificURL string) Credentials {
	return Credentials{
		ClientID:          clientID,
		ClientSecret:      clientSecret,
		TenantSpecificURL: tenantSpecificURL,
	}
}

func (secrets *Secrets) AddCredentials(credentials Credentials) error {
	secrets.stored = credentials
	return nil
}

func (secrets *Secrets) GetCredentials() (Credentials, bool, error) {

	return secrets.stored, secrets.stored.ClientID != "", nil
}
