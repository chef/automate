package main

type deployDirector struct {
	manager deployManager
}

func newDeployDirector(m deployManager) *deployDirector {
	return &deployDirector{
		manager: m,
	}
}

func (d *deployDirector) setDeployManager(m deployManager) {
	d.manager = m
}

func (d *deployDirector) executeDeployemnt(args []string) error {
	return d.manager.doDeployWork(args)
}

func (d *deployDirector) executeProvisionInfra(args []string) error {
	return d.manager.doProvisionJob(args)
}
