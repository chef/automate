package main

type MockBackupFromBashtionImp struct {
	isBastionHostFunc                func() bool
	executeOnRemoteAndPoolStatusFunc func(commandString string, infra *AutomteHAInfraDetails) error
}

func (mbfb *MockBackupFromBashtionImp) isBastionHost() bool {
	return mbfb.isBastionHostFunc()
}

func (mbfb *MockBackupFromBashtionImp) executeOnRemoteAndPoolStatus(commandString string, infra *AutomteHAInfraDetails) error {
	return mbfb.executeOnRemoteAndPoolStatusFunc(commandString, infra)
}
