package statusservice

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

type IStatusService interface {
	GetServices() ([]models.ServiceDetails, error)
	GetServicesFromAutomateStatus() (map[string]models.ServiceDetails, error)
	GetServicesFromHabSvcStatus() ([]models.ServiceDetails, error)
	ParseChefAutomateStatus(output string) (map[string]models.ServiceDetails, error)
	ParseHabSvcStatus(output string) ([]models.ServiceDetails, error)
}

type StatusService struct {
	ExecuteShellCommandFunc func(name string, arg []string) ([]byte, error)
}

func NewStatusService(executeShellCommand func(name string, arg []string) ([]byte, error)) IStatusService {
	return &StatusService{
		ExecuteShellCommandFunc: executeShellCommand,
	}
}

func (ss *StatusService) GetServices() ([]models.ServiceDetails, error) {
	ss.GetServicesFromHabSvcStatus()
	return []models.ServiceDetails{}, nil
}

func (ss *StatusService) GetServicesFromHabSvcStatus() ([]models.ServiceDetails, error) {
	output, err := ss.ExecuteShellCommandFunc("HAB_LICENSE=accept-no-persist hab", []string{"svc", "status"})
	fmt.Println("Output: ", string(output))
	fmt.Println("Error: ", err)
	if err != nil {
		return nil, fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}

	response, err := ss.ParseHabSvcStatus(string(output))
	if err != nil {
		return nil, err
	}

	automateResponse, err := ss.GetServicesFromAutomateStatus()
	if err != nil {
		return nil, err
	}

	for i, service := range response {
		if automateService, ok := automateResponse[service.ServiceName]; ok {
			response[i].Status = automateService.Status
		}
	}
	return response, nil
}

func (ss *StatusService) ParseHabSvcStatus(output string) ([]models.ServiceDetails, error) {
	response := make([]models.ServiceDetails, 0)

	tableStart := strings.Index(output, "package")
	if tableStart == -1 {
		return nil, fiber.NewError(fiber.StatusInternalServerError, "no table found in output")
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
			response = append(response, models.ServiceDetails{
				ServiceName: serviceName,
				Status:      matches[4],
				Version:     matches[1],
			})
		}
	}
	fmt.Println(response)
	return response, nil
}

func (ss *StatusService) GetServicesFromAutomateStatus() (map[string]models.ServiceDetails, error) {
	output, err := ss.ExecuteShellCommandFunc("chef-automate", []string{"status"})
	fmt.Println("Output: ", string(output))
	fmt.Println("Error: ", err)
	if err != nil {
		return nil, fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}
	return ss.ParseChefAutomateStatus(string(output))
}

func (ss *StatusService) ParseChefAutomateStatus(output string) (map[string]models.ServiceDetails, error) {
	response := make(map[string]models.ServiceDetails)

	tableStart := strings.Index(output, "Service Name")
	if tableStart == -1 {
		return nil, fiber.NewError(fiber.StatusInternalServerError, "no table found in output")
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
				Status:      matches[3],
				Version:     "",
			}
		}
	}
	fmt.Println(response)
	return response, nil
}
