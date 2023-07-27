package v1

import (
	"net/http"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) FirewallCheck(c *fiber.Ctx) error {
	reqBody := models.FirewallRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	// If any field is empty or contains only space
	if strings.TrimSpace(reqBody.SourceNodeIP) == "" ||
		strings.TrimSpace(reqBody.DestinationNodeIP) == "" ||
		strings.TrimSpace(reqBody.DestinationServicePort) == "" ||
		strings.TrimSpace(reqBody.DestinationServiceProtocol) == "" {
		return fiber.NewError(http.StatusBadRequest, "source_node_ip, destination_node_ip, destination_service_port or destination_service_protocol cannot be empty")
	}

	// port number is not a integer
	port, err := strconv.Atoi(strings.TrimSpace(reqBody.DestinationServicePort))
	if err != nil {
		h.Logger.Error(err)
		return fiber.NewError(http.StatusBadRequest, "Invalid destination_service_port number")
	}

	// If port number is invalid
	if !isValidPort(port) {
		h.Logger.Error("Request body contains invalid port number")
		return fiber.NewError(http.StatusBadRequest, "Invalid destination_service_port range")
	}

	// sourceIp is same as destinationIP
	if strings.TrimSpace(reqBody.SourceNodeIP) == strings.TrimSpace(reqBody.DestinationNodeIP) {
		return fiber.NewError(http.StatusBadRequest, "source_node_ip and destination_node_ip cannot be same")
	}

	// Supported protocol check
	if strings.TrimSpace(reqBody.DestinationServiceProtocol) != constants.TCP &&
		strings.TrimSpace(reqBody.DestinationServiceProtocol) != constants.UDP &&
		strings.TrimSpace(reqBody.DestinationServiceProtocol) != constants.HTTPS {
		return fiber.NewError(http.StatusBadRequest, "Please Give Valid Protocol i.e tcp, udp or https")
	}

	firewallDetails := h.FirewallService.GetFirewallDetails(reqBody)
	return c.JSON(response.BuildSuccessResponse(firewallDetails))
}
