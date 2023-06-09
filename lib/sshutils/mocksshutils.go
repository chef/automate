package sshutils

type MockSSHUtilsImpl struct {
	Executefunc func(sshConfig SSHConfig, cmd string) (string, error)
}

func (mssh *MockSSHUtilsImpl) Execute(sshConfig SSHConfig, cmd string) (string, error) {
	return mssh.Executefunc(sshConfig, cmd)
}
