package phase1_migration

type KeyDump struct {
	ID                            string      `json:"id"`
	AuthzID                       string      `json:"authz_id"`
	Username                      string      `json:"username"`
	Email                         string      `json:"email"`
	PubkeyVersion                 int         `json:"pubkey_version"`
	PublicKey                     interface{} `json:"public_key"`
	SerializedObject              string      `json:"serialized_object"`
	LastUpdatedBy                 string      `json:"last_updated_by"`
	CreatedAt                     string      `json:"created_at"`
	UpdatedAt                     string      `json:"updated_at"`
	ExternalAuthenticationUID     interface{} `json:"external_authentication_uid"`
	RecoveryAuthenticationEnabled interface{} `json:"recovery_authentication_enabled"`
	Admin                         bool        `json:"admin"`
	HashedPassword                interface{} `json:"hashed_password"`
	Salt                          interface{} `json:"salt"`
	HashType                      interface{} `json:"hash_type"`
}

type Org struct {
	Name     string `json:"name"`
	FullName string `json:"full_name"`
	GUID     string `json:"guid"`
}

type User struct {
	Username    string `json:"username"`
	Email       string `json:"email"`
	DisplayName string `json:"display_name"`
	FirstName   string `json:"first_name"`
	LastName    string `json:"last_name"`
	MiddleName  string `json:"middle_name"`
	PublicKey   string `json:"public_key"`
	IsAdmin     bool
}
