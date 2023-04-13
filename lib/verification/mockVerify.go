package verification

type VerificationMock struct {
	VerifyHAAWSProvisionFunc              func(configFile string) error
	VerifyHAAWSManagedProvisionFunc       func(configFile string) error
	VerifyHAAWSDeploymentFunc             func(configFile string) error
	VerifyHAAWSManagedDeploymentFunc      func(configFile string) error
	VerifyOnPremDeploymentFunc            func(configFile string) error
	VerifyOnPremAWSManagedDeploymentFunc  func(configFile string) error
	VerifyOnPremCustManagedDeploymentFunc func(configFile string) error
	VerifyStandaloneDeploymentFunc        func(configFile string) error
	VerifyCertificatesFunc                func(configFile string) error
}

func (v *VerificationMock) VerifyHAAWSProvision(configFile string) error {
	return v.VerifyHAAWSProvisionFunc(configFile)
}

func (v *VerificationMock) VerifyHAAWSManagedProvision(configFile string) error {
	return v.VerifyHAAWSManagedProvisionFunc(configFile)
}

func (v *VerificationMock) VerifyHAAWSDeployment(configFile string) error {
	return v.VerifyHAAWSDeploymentFunc(configFile)
}

func (v *VerificationMock) VerifyHAAWSManagedDeployment(configFile string) error {
	return v.VerifyHAAWSManagedDeploymentFunc(configFile)
}

func (v *VerificationMock) VerifyOnPremDeployment(configFile string) error {
	return v.VerifyOnPremDeploymentFunc(configFile)
}

func (v *VerificationMock) VerifyOnPremAWSManagedDeployment(configFile string) error {
	return v.VerifyOnPremAWSManagedDeploymentFunc(configFile)
}

func (v *VerificationMock) VerifyOnPremCustManagedDeployment(configFile string) error {
	return v.VerifyOnPremCustManagedDeploymentFunc(configFile)
}

func (v *VerificationMock) VerifyStandaloneDeployment(configFile string) error {
	return v.VerifyStandaloneDeploymentFunc(configFile)
}

func (v *VerificationMock) VerifyCertificates(configFile string) error {
	return v.VerifyCertificatesFunc(configFile)
}
