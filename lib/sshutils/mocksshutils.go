package sshutils

type MockSSHUtilsImpl struct {
	Executefunc      func(sshConfig SSHConfig, cmd string) (string, error)
	NewSshConfigfunc func(ip string, port string, keyFile string, userName string) SSHConfig
}

func (mssh *MockSSHUtilsImpl) Execute(sshConfig SSHConfig, cmd string) (string, error) {
	return mssh.Executefunc(sshConfig, cmd)
}

func (mssh *MockSSHUtilsImpl) NewSshConfig(ip string, port string, keyFile string, userName string) SSHConfig {
	return mssh.NewSshConfigfunc(ip, port, keyFile, userName)
}
