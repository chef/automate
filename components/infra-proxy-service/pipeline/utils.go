package pipeline

func (usr *User) SetConnector(uid interface{}) {
	if uid == nil {
		usr.Connector = Local
	} else {
		usr.Connector = LADP
	}
}

func (usr *User) SetAutomateUsername(uid interface{}) {
	if uid == nil {
		usr.AutomateUsername = usr.Username
	} else {
		usr.Connector = uid.(string)
	}
}
