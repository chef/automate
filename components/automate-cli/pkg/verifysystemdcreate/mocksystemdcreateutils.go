package verifysystemdcreate

type MockSystemdCreateUtils struct {
	GetBinaryPathFunc            func() (string, error)
	SystemdRunningFunc           func() error
	CreateDestinationAndCopyFunc func(binarySrcPath, binaryDestPath string) error
	ExecuteShellCommandFunc      func(name string, arg []string) error
	GetEnvFunc                   func() string
}

func (msc *MockSystemdCreateUtils) GetBinaryPath() (string, error) {
	return msc.GetBinaryPathFunc()
}
func (msc *MockSystemdCreateUtils) SystemdRunning() error {
	return msc.SystemdRunningFunc()
}
func (msc *MockSystemdCreateUtils) CreateDestinationAndCopy(binarySrcPath, binaryDestPath string) error {
	return msc.CreateDestinationAndCopyFunc(binarySrcPath, binaryDestPath)
}
func (msc *MockSystemdCreateUtils) ExecuteShellCommand(name string, arg []string) error {
	return msc.ExecuteShellCommandFunc(name, arg)
}
func (msc *MockSystemdCreateUtils) GetEnv() string {
	return msc.GetEnvFunc()
}