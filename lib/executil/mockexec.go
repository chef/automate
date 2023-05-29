package executil

type ExecCmdServiceMock struct {
	CommandFunc func(name string, args []string) ([]byte, error)
}

func (s *ExecCmdServiceMock) Command(name string, args []string) ([]byte, error) {
	return s.CommandFunc(name, args)
}