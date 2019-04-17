package server

import (
	"context"
	"errors"
	"math"
	"time"

	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

// RestartServices implements the RestartServices RPC. It will restart all the services
// the deployment service set to running
func (s *server) RestartServices(context.Context, *api.RestartServicesRequest) (*api.RestartServicesResponse, error) {
	logrus.Info("Restarting services")
	taskID, err := s.restartServices()
	if err != nil {
		return nil, err
	}
	return &api.RestartServicesResponse{TaskId: taskID}, nil
}

// restartServices restarts all non-deployment-service services. To
// avoid concurrent runs of a converge loop, it stops the converge
// loop during the restart process.
func (s *server) restartServices() (string, error) {
	s.deployment.Lock()
	defer s.deployment.Unlock()

	// Initialize both these tasks early to avoid failing in the
	// middle of the restart process because of a failure to
	// generate a Task.
	taskStop, err := converge.NewTask()
	if err != nil {
		return "", err
	}

	taskStart, err := converge.NewTask()
	if err != nil {
		return "", err
	}

	// Starting the converge loop will bring up all of the
	// services. We stop it here to avoid that.
	//
	// It is possible the user could re-enable the converge loop
	// manually, which would restart our services before we
	// expected them to be restarted.
	s.convergeLoop.Stop()
	defer s.convergeLoop.Start()

	logger := newConvergeLoopLogger()
	desiredStateStopped, servicesGoingDown := s.buildStopDesiredState()
	err = s.converger.Converge(0, taskStop, desiredStateStopped, logger)
	if err != nil {
		return "", err
	}

	<-taskStop.C

	// Habitat operates asynchronously. As a result, services may
	// still be up after our stop task returns. waitForDown checks
	// the actual service state of the services.
	err = s.waitForDown(servicesGoingDown)
	if err != nil {
		return "", err
	}

	sender := s.newEventSender()
	s.senderStore.Set(taskStart.ID.String(), sender)

	sender.Deploy(api.Running)

	// startConverge will use the existing expected state of the
	// services to build a new desired state for the converger.
	// This should result in the restart of any services marked as
	// Running.
	eDeploy := &errDeployer{server: s, err: nil, sender: sender}
	eDeploy.startConverge(taskStart, newEventAdapter(sender))

	go func() {
		defer sender.TaskComplete()
		eDeploy.waitForConverge(taskStart)
		eDeploy.ensureStatus(context.Background(), servicesGoingDown, s.ensureStatusTimeout)

		if eDeploy.err != nil {
			sender.Deploy(api.CompleteFail)
			logrus.WithError(eDeploy.err).Error("Failed to restart services")
		}
	}()

	return taskStart.ID.String(), nil
}

func (s *server) waitForDown(services []string) error {
	ctx := context.Background()
	for i := 0; i <= 5; i++ {
		logrus.WithField("try", i).Info("Waiting for services to go down")
		backoff := time.Duration(math.Pow(float64(2), float64(i)))
		time.Sleep(backoff * time.Second)

		deployed, err := s.target().DeployedServices(ctx)
		if err != nil {
			logrus.WithError(err).Warn("Failed to list services")
		}

		if allDown(services, deployed) {
			return nil
		}
	}
	logrus.Warn("Services did not go down")
	return errors.New("Services did not go down")
}

func allDown(services []string, deployed map[string]target.DeployedService) bool {
	for _, svc := range services {
		for _, deployedSvc := range deployed {
			if svc == deployedSvc.Pkg.Name() {
				return false
			}
		}
	}
	return true
}

func (s *server) buildStopDesiredState() (converge.DesiredState, []string) {
	topology := make(converge.Topology)
	topology[s.target()] = []converge.Service{
		{
			Name:          deploymentServiceName,
			ConvergeState: converge.Skip(),
		},
	}

	servicesGoingDown := []string{}
	for _, svc := range s.deployment.ExpectedServices {
		if svc.DeploymentState == deployment.Running && svc.Name() != deploymentServiceName {
			logrus.WithFields(logrus.Fields{
				"name":  svc.Name(),
				"state": svc.DeploymentState,
			}).Info("Expecting service to be down")
			servicesGoingDown = append(servicesGoingDown, svc.Name())
		}
	}

	return converge.NewDesiredState(topology,
			converge.NewSkipSupervisorState(),
			s.deployment.CurrentReleaseManifest.ListPackages(),
			depot.DisabledGC),
		servicesGoingDown
}
