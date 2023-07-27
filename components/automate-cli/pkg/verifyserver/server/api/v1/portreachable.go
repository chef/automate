package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) PortReachable(c *fiber.Ctx) error {
	reqBody := models.PortReachableRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}
	if reqBody.DestinationNodeIp == "" ||
		reqBody.DestinationNodePort == 0 ||
		reqBody.DestinationNodeServiceProtocol == "" {
		return fiber.NewError(http.StatusBadRequest, "DestinationNodeIp, DestinationNodePort, or DestinationNodeServiceProtocol cannot be empty")
	}

	// If port number is invalid
	if !isValidPort(reqBody.DestinationNodePort) {
		h.Logger.Error("Request body contains invalid port number")
		return fiber.NewError(http.StatusBadRequest, "Invalid port number")
	}

	if reqBody.DestinationNodeServiceProtocol != constants.TCP && reqBody.DestinationNodeServiceProtocol != constants.UDP && reqBody.DestinationNodeServiceProtocol != constants.HTTPS {
		return fiber.NewError(http.StatusBadRequest, "Please Give Valid Protocol i.e tcp, udp or https")
	}
	portReachableResult := h.PortReachableService.GetPortReachableDetails(reqBody)
	return c.JSON(response.BuildSuccessResponse(portReachableResult))
}
