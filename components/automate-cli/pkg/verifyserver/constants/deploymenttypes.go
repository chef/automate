package constants

type DeploymentState string

const (
	PRE_DEPLOY  = "pre-deploy"
	POST_DEPLOY = "post-deploy"
)
const (
	DeploymentStatePreDeploy  DeploymentState = PRE_DEPLOY
	DeploymentStatePostDeploy DeploymentState = POST_DEPLOY
)
