package main

type existingInfra struct{}

func newExistingInfa() *existingInfra {
	return &existingInfra{}
}

func (e *existingInfra) doDeployWork(args []string) error {
	err := bootstrapEnv(args)
	err = deployA2HA(args)
	return err
}

func (e *existingInfra) doProvisionJob(args []string) error {
	return nil
}
