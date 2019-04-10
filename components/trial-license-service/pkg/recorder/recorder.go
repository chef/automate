package recorder

// Client allows recording client data
type Client interface {
	Record(licenseID, customerID, name, email,
		automateVersion, deploymentID string, gdprAgree bool) error
}

type noop struct{}

// NoopRecorder returns a recorder that records nothing. For testing.
func NoopRecorder() Client {
	return &noop{}
}

func (*noop) Record(string, string, string, string, string, string, bool) error {
	return nil
}
