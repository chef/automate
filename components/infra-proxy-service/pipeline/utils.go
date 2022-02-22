package pipeline

func (usr *User) SetConnector(uid interface{}) {
	if uid == nil {
		usr.Connector = usr.Username
	} else {
		usr.Connector = uid.(string)
	}
}
