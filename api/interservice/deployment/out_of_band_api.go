package deployment

const (
	// ConvergeDisableFilePath is the path to the converge loop disable sentinel
	// file. If the file exists the deployment-service won't start the converge
	// when it starts up. This will allow us to execute a restoration before
	// the deployment service tries to converge the expected services.
	ConvergeDisableFilePath = "/hab/svc/deployment-service/data/converge_disable"
)
