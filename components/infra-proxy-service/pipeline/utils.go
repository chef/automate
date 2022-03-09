package pipeline

// Set Connector field
func (usr *User) SetConnector(uid interface{}) {
	if uid == nil {
		usr.Connector = Local
	} else {
		usr.Connector = LDAP
	}
}

// Set AutomateUsername field
func (usr *User) SetAutomateUsername(uid interface{}) {
	if uid == nil {
		usr.AutomateUsername = usr.Username
	} else {
		usr.AutomateUsername = uid.(string)
	}
}
