package stopmockserverservice

import (
	"context"
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IStopMockServerService interface {
	StopMockServer(server *models.Server) error
	StopTCPServer(server *models.Server) error
	StopUDPServer(server *models.Server) error
	StopHTTPSServer(server *models.Server) error
}

// StopMockServerService provides functionality to stop mock servers.
type StopMockServerService struct {
	Logger logger.Logger
}

func NewStopMockServerService(logger logger.Logger) *StopMockServerService {
	return &StopMockServerService{
		Logger: logger,
	}
}

// StopMockServer stops a mock server of the given type and port.
func (sm *StopMockServerService) StopMockServer(server *models.Server) error {
	var err error
	switch server.Protocol {
	case constants.TCP:
		err = sm.StopTCPServer(server)
	case constants.UDP:
		err = sm.StopUDPServer(server)
	case constants.HTTPS:
		err = sm.StopHTTPSServer(server)
	}
	if err != nil {
		return err
	}
	return nil
}

func (sm *StopMockServerService) StopTCPServer(server *models.Server) error {

	server.SignalChan <- true
	if err := server.ListenerTCP.Close(); err != nil {
		errString := fmt.Sprintf("Error while stopping TCP server: %v", err.Error())
		sm.Logger.Error(fmt.Errorf(errString))
		return err
	}

	return nil
}

func (sm *StopMockServerService) StopUDPServer(server *models.Server) error {

	server.SignalChan <- true
	if err := server.ListenerUDP.Close(); err != nil {
		errString := fmt.Sprintf("Error while stopping UDP server: %v", err.Error())
		sm.Logger.Error(fmt.Errorf(errString))
		return err
	}

	return nil
}

func (sm *StopMockServerService) StopHTTPSServer(server *models.Server) error {

	if err := server.ListenerHTTP.Shutdown(context.Background()); err != nil {
		errString := fmt.Sprintf("Error while stopping HTTPS server: %v", err.Error())
		sm.Logger.Error(fmt.Errorf(errString))
		return err
	}

	return nil
}
