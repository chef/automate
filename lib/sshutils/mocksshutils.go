package sshutils

type MockSSHUtilsImpl struct {
	Executefunc                      func(sshConfig SSHConfig, cmd string) (string, error)
	ExecuteConcurrentlyFunc          func(sshConfig SSHConfig, cmd string, hostIPs []string) []Result
	CopyFileToRemoteFunc             func(sshConfig SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool) error
	CopyFileToRemoteConcurrentlyFunc func(sshConfig SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []Result
}

func (mssh *MockSSHUtilsImpl) Execute(sshConfig SSHConfig, cmd string) (string, error) {
	return mssh.Executefunc(sshConfig, cmd)
}

func (mssh *MockSSHUtilsImpl) ExecuteConcurrently(sshConfig SSHConfig, cmd string, hostIPs []string) []Result {
	return mssh.ExecuteConcurrentlyFunc(sshConfig, cmd, hostIPs)
}

func (mssh *MockSSHUtilsImpl) CopyFileToRemote(sshConfig SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool) error {
	return mssh.CopyFileToRemoteFunc(sshConfig, srcFilePath, destFileName, destDir, removeFile)
}

func (mssh *MockSSHUtilsImpl) CopyFileToRemoteConcurrently(sshConfig SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []Result {
	return mssh.CopyFileToRemoteConcurrentlyFunc(sshConfig, srcFilePath, destFileName, destDir, removeFile, hostIPs)
}
