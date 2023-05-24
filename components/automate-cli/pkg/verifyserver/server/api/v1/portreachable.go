package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) PortReachable(c *fiber.Ctx) {
	reqBody := models.PortReachableRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"})
		return
	}
	if reqBody.DestinationNodeIp == "" ||
		reqBody.DestinationNodePort == 0 ||
		reqBody.DestinationNodeServiceProtocol == "" {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "DestinationNodeIp, DestinationNodePort, or DestinationNodeServiceProtocol cannot be empty"})
		return
	}

	// If port number is invalid
	err := validatePortRange(reqBody.DestinationNodePort, h)
	if err != nil {
		h.Logger.Error(err)
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid port number"})
		return
	}

	if reqBody.DestinationNodeServiceProtocol == constants.HTTPS && reqBody.RootCA == "" {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "RootCA value is mandatory for protocol HTTPS"})
		return
	}
	if reqBody.DestinationNodeServiceProtocol != constants.TCP && reqBody.DestinationNodeServiceProtocol != constants.UDP && reqBody.DestinationNodeServiceProtocol != constants.HTTPS {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Please Give Valid Protocol i.e tcp, udp or https"})
		return
	}
	portReachableResult := h.PortReachableService.GetPortReachableDetails(reqBody)
	c.JSON(response.BuildSuccessResponse(portReachableResult))
}
