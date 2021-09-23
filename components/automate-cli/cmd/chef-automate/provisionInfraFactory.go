package main

type provisionInfraDirector struct {
	manager provisionInfraManager
}

func newProvisionInfraDirector(m provisionInfraManager) *provisionInfraDirector {
	return &provisionInfraDirector{
		manager: m,
	}
}

func (d *provisionInfraDirector) setProvisionInfraManager(m provisionInfraManager) {
	d.manager = m
}

func (d *provisionInfraDirector) executeProvisionInfra(args []string) error {
	return d.manager.doProvisionJob(args)
}
