package pipeline

func (usr User) SetConnector(uid interface{}) {
	if uid == nil {
		usr.Connector = Local
	} else {
		usr.Connector = uid.(string)
	}
}
