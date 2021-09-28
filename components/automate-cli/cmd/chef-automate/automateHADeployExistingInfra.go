package main

type existingInfra struct{}

func newExistingInfa() *existingInfra {
	return &existingInfra{}
}

func (e *existingInfra) doDeployWork(args []string) error {
	var err = bootstrapEnv(args)
	if err != nil {
		err = deployA2HA()
	}
	return err
}

func (e *existingInfra) doProvisionJob(args []string) error {
	return nil
}
