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
	if strings.TrimSpace(reqBody.SourceNodeIP) == "" ||
		strings.TrimSpace(reqBody.DestinationNodeIP) == "" ||
		strings.TrimSpace(reqBody.DestinationServicePort) == "" ||
		strings.TrimSpace(reqBody.DestinationServiceProtocol) == "" {
		return fiber.NewError(http.StatusBadRequest, "source_node_ip, destination_node_ip, destination_service_port, destination_service_protocol, cert, key or root_cert cannot be empty")
	}

	port, err := strconv.Atoi(reqBody.DestinationServicePort)
	if err != nil {
		h.Logger.Error(err)
		return fiber.NewError(http.StatusBadRequest, "Invalid port number")
	}

	// If port number is invalid
	err = validatePortRange(port, h)
	if err != nil {
		h.Logger.Error(err)
		return fiber.NewError(http.StatusBadRequest, "Invalid port range")
	}

	if reqBody.DestinationServiceProtocol == constants.HTTPS && reqBody.RootCert == "" {
		return fiber.NewError(http.StatusBadRequest, "RootCA value is mandatory for protocol HTTPS")
	}
	if reqBody.DestinationServiceProtocol != constants.TCP && reqBody.DestinationServiceProtocol != constants.UDP && reqBody.DestinationServiceProtocol != constants.HTTPS {
		return fiber.NewError(http.StatusBadRequest, "Please Give Valid Protocol i.e tcp, udp or https")
	}
	firewallDetails := h.FirewallService.GetFirewallDetails(reqBody)
	return c.JSON(response.BuildSuccessResponse(firewallDetails))
}
