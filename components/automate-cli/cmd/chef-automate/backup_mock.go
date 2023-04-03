package main

type MockBackupFromBashtionImp struct {
	isBastionHostFunc                func() bool
	executeOnRemoteAndPoolStatusFunc func(commandString string, infra *AutomateHAInfraDetails, pooling bool, stopFrontends bool, backupState bool) error
}

func (mbfb *MockBackupFromBashtionImp) isBastionHost() bool {
	return mbfb.isBastionHostFunc()
}

func (mbfb *MockBackupFromBashtionImp) executeOnRemoteAndPoolStatus(commandString string, infra *AutomateHAInfraDetails, pooling bool, stopFrontends bool, backupState bool) error {
	return mbfb.executeOnRemoteAndPoolStatusFunc(commandString, infra, pooling, stopFrontends, backupState)
}
