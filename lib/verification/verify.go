package verification

type verifySetup interface {
	VerifyHAAWSProvision(string)
	VerifyHAAWSManagedProvision(string)
	VerifyHAAWSDeployment(string)
	VerifyHAAWSManagedDeployment(string)
	VerifyOnPremDeployment(string)
	VerifyOnPremAWSManagedDeployment(string)
	VerifyOnPremCustManagedDeployment(string)
	VerifyStandaloneDeployment(string)
	VerifyCertificates(string)
}

func VerifyHAAWSProvision(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}

	validateAWSProvisionConfig(config)
	return nil
}

func VerifyHAAWSManagedProvision(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedProvisionConfig(config)
	return nil
}

func VerifyHAAWSDeployment(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSDeploymentConfig(config)
	return nil
}

func VerifyHAAWSManagedDeployment(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedDeploymentConfig(config)
	return nil
}

func VerifyOnPremDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremConfig(config)
	return nil
}

func VerifyOnPremAWSManagedDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremAWSConfig(config)
	return nil
}

func VerifyOnPremCustManagedDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremCustomerConfig(config)
	return nil
}

func VerifyStandaloneDeployment(configFile string) error {
	config, err := parseStandaloneConfig(configFile)
	if err != nil {
		return err
	}
	validateStandaloneDeploymentConfig(config)
	return nil
}

func VerifyCertificates(certContents string) error {
	validateCertificateExpiry(certContents)
	return nil
}
