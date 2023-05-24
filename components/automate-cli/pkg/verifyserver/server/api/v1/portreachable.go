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
		// c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"})
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}
	if reqBody.DestinationNodeIp == "" ||
		reqBody.DestinationNodePort == 0 ||
		reqBody.DestinationNodeServiceProtocol == "" {
		// c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "DestinationNodeIp, DestinationNodePort, or DestinationNodeServiceProtocol cannot be empty"})
		return fiber.NewError(http.StatusBadRequest, "DestinationNodeIp, DestinationNodePort, or DestinationNodeServiceProtocol cannot be empty")
	}

	// If port number is invalid
	err := validatePortRange(reqBody.DestinationNodePort, h)
	if err != nil {
		h.Logger.Error(err)
		// c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid port number"})
		return fiber.NewError(http.StatusBadRequest, "Invalid port number")
	}

	if reqBody.DestinationNodeServiceProtocol == constants.HTTPS && reqBody.RootCA == "" {
		// c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "RootCA value is mandatory for protocol HTTPS"})
		return fiber.NewError(http.StatusBadRequest, "RootCA value is mandatory for protocol HTTPS")
	}
	if reqBody.DestinationNodeServiceProtocol != constants.TCP && reqBody.DestinationNodeServiceProtocol != constants.UDP && reqBody.DestinationNodeServiceProtocol != constants.HTTPS {
		// c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Please Give Valid Protocol i.e tcp, udp or https"})
		return fiber.NewError(http.StatusBadRequest, "Please Give Valid Protocol i.e tcp, udp or https")
	}
	portReachableResult := h.PortReachableService.GetPortReachableDetails(reqBody)
	return c.JSON(response.BuildSuccessResponse(portReachableResult))
}
