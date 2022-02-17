package pipeline

type ActionOps int

const (
	Insert ActionOps = 1 + iota
	Skip
	Delete
	Update
)

//
type Result struct {
	// Meta for Zip file info
	Meta Meta `json:"meta"`

	// ParsedResult for Orgs, Users
	ParsedResult ParsedResult `json:"parsed_result"`
}

type Meta struct {
	// StageResults holds Skipped instances
	StageResults []StageResult `json:"stage_results"`

	// ZipFile for zip file location
	ZipFile string `json:"zip_file"`

	// UnzipFolder for unzipped folder's location
	UnzipFolder string `json:"unzip_folder"`

	// Chef Infra Server ID
	ServerID string `json:"server_id"`

	// Migration ID
	MigrationID string `json:"migration_id"`
}

type StageResult struct {
	StageName string `json:"stage_name"`
	IsSuccess bool   `json:"is_success"`
	Failure   error  `json:"failure"`
}

type ParsedResult struct {
	// Orgs array
	Orgs []Org `json:"orgs"`

	// Users array
	Users []User `json:"users"`

	// OrgsUsers for Orgs and Users associations
	OrgsUsers []OrgsUsersAssociations `json:"orgs_users_associations"`
}

// OrgsUsersAssociations
type OrgsUsersAssociations struct {
	// OrgName
	OrgName Org `json:"org_name"`

	// Users UserAssociation slice
	Users []UserAssociation `json:"user_association"`
}

type UserAssociation struct {
	// Username
	Username string `json:"username"`

	// IsAdmin
	IsAdmin bool `json:"is_admin"`

	// ActionOps for Insert Skip Update and Delete for UserAssociation
	ActionOps ActionOps `json:"action_ops"`
}

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

	// Org Name
	Name string `json:"name"`

	// FullName
	FullName string `json:"full_name"`

	// ActionOps for Insert Skip Update and Delete
	ActionOps ActionOps `json:"action_ops"`
}

type User struct {
	Username    string `json:"username"`
	Email       string `json:"email"`
	DisplayName string `json:"display_name"`
	FirstName   string `json:"first_name"`
	LastName    string `json:"last_name"`
	MiddleName  string `json:"middle_name"`

	// AutomateUsername is ldap username
	AutomateUsername string `json:"automate_username"`

	// Connector ldap user
	Connector string `json:"connector"`

	// IsConflicting for user's existence in db
	IsConflicting bool `json:"is_conflicting"`

	// IsAdmin (user is admin or not)
	IsAdmin bool `json:"is_admin"`
}

type OrgJson struct {
	Name     string `json:"name"`
	FullName string `json:"full_name"`
	Guid     string `json:"guid"`
}

type MemberJson []struct {
	User UserJson `json:"user"`
}

type UserJson struct {
	Username string `json:"username"`
}
