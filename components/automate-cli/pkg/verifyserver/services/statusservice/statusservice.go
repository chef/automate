package statusservice

import (
	"regexp"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
)

type IStatusService interface {
	GetServices() ([]models.ServiceDetails, error)
	GetServicesFromAutomateStatus() (map[string]models.ServiceDetails, error)
	GetServicesFromHabSvcStatus() ([]models.ServiceDetails, error)
	ParseChefAutomateStatus(output string) (map[string]models.ServiceDetails, error)
	ParseHabSvcStatus(output string) ([]models.ServiceDetails, error)
	CheckIfBENode(output string) bool
}

type StatusService struct {
	ExecuteShellCommandFunc func(cmd string) ([]byte, error)
	Log                     logger.Logger
}

const (
	AutomateStatusOnBE = "FileAccessError: Unable to access the file or directory: Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory"
)

func NewStatusService(executeShellCommand func(cmd string) ([]byte, error), log logger.Logger) IStatusService {
	return &StatusService{
		ExecuteShellCommandFunc: executeShellCommand,
		Log:                     log,
	}
}

func (ss *StatusService) GetServices() ([]models.ServiceDetails, error) {
	habResponse, err := ss.GetServicesFromHabSvcStatus()
	if err != nil {
		return nil, err
	}

	automateResponse, err := ss.GetServicesFromAutomateStatus()
	if err != nil {
		return nil, err
	}

	for i, service := range habResponse {
		if automateService, ok := automateResponse[service.ServiceName]; ok {
			habResponse[i].Status = automateService.Status
		}
	}
	return habResponse, nil
}

// Get the services from
func (ss *StatusService) GetServicesFromHabSvcStatus() ([]models.ServiceDetails, error) {
	output, err := ss.ExecuteShellCommandFunc("HAB_LICENSE=accept-no-persist hab svc status")
	ss.Log.Debug("Output for 'HAB_LICENSE=accept-no-persist hab svc status' command: ", string(output))
	if err != nil {
		ss.Log.Error("Error while running 'HAB_LICENSE=accept-no-persist hab svc status' command: ", err)
		return nil, fiber.NewError(fiber.StatusInternalServerError, "Error getting services from hab svc status")
	}
	return ss.ParseHabSvcStatus(string(output))
}

// Parse the output of hab svc status
func (ss *StatusService) ParseHabSvcStatus(output string) ([]models.ServiceDetails, error) {
	response := make([]models.ServiceDetails, 0)

	tableStart := strings.Index(output, "package")
	if tableStart == -1 {
		return nil, fiber.NewError(fiber.StatusInternalServerError, "No table found in output")
	}

	lines := strings.Split(output[tableStart:], "\n")
	for i, line := range lines {
		if i == 0 || line == "" {
			continue
		}
		regex := regexp.MustCompile(`(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)`)
		matches := regex.FindStringSubmatch(line)
		if len(matches) == 8 {
			serviceName := strings.Split(matches[7], ".default")[0]
			status := strings.ToUpper(matches[4])
			if status == constants.UP {
				status = constants.OK
			} else if status == constants.DOWN {
				status = constants.CRITICAL
			}
			response = append(response, models.ServiceDetails{
				ServiceName: serviceName,
				Status:      status,
				Version:     matches[1],
			})
		}
	}
	return response, nil
}

// Get the services from chef-automate status
func (ss *StatusService) GetServicesFromAutomateStatus() (map[string]models.ServiceDetails, error) {
	output, err := ss.ExecuteShellCommandFunc("chef-automate status")
	ss.Log.Debug("Output for 'chef-automate status' command: ", string(output))
	if err != nil {
		ss.Log.Error("Error while running 'chef-automate status' command: ", err)
		//If it's a backend node, return an empty map
		if ss.CheckIfBENode(string(output)) {
			return make(map[string]models.ServiceDetails), nil
		}
		return nil, fiber.NewError(fiber.StatusInternalServerError, "Error getting services from chef-automate status")
	}
	return ss.ParseChefAutomateStatus(string(output))
}

// Parse the output of chef-automate status
func (ss *StatusService) ParseChefAutomateStatus(output string) (map[string]models.ServiceDetails, error) {
	response := make(map[string]models.ServiceDetails)

	tableStart := strings.Index(output, "Service Name")
	if tableStart == -1 {
		return nil, fiber.NewError(fiber.StatusInternalServerError, "No table found in output")
	}

	lines := strings.Split(output[tableStart:], "\n")
	for i, line := range lines {
		if i == 0 || line == "" {
			continue
		}
		regex := regexp.MustCompile(`(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)`)
		matches := regex.FindStringSubmatch(line)
		if len(matches) == 6 {
			response[matches[1]] = models.ServiceDetails{
				ServiceName: matches[1],
				Status:      strings.ToUpper(matches[3]),
				Version:     "",
			}
		}
	}
	return response, nil
}

// Check if it's a backend node
func (ss *StatusService) CheckIfBENode(output string) bool {
	return strings.Contains(output, AutomateStatusOnBE)
}
