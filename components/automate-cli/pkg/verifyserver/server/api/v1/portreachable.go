package v1

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) PortReachable(c *fiber.Ctx) {
	reqBody := models.PortReachableRequest{}
	fmt.Println(reqBody)
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"})
		return
	}
	if reqBody.DestinationNodeIp == "" ||
		reqBody.DestinationNodePort == 0 ||
		reqBody.DestinationNodeServiceProtocol == "" {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Required all Body Parameter"})
		return
	}
	if reqBody.DestinationNodeServiceProtocol == constants.HTTPS && reqBody.RootCA == "" {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Give root CA"})
		return
	}
	if reqBody.DestinationNodeServiceProtocol != constants.TCP && reqBody.DestinationNodeServiceProtocol != constants.UDP && reqBody.DestinationNodeServiceProtocol != constants.HTTPS {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Protocol"})
		return
	}
	portReachableResult := h.PortReachableService.GetPortReachableDetails(reqBody)
	c.JSON(response.BuildSuccessResponse(portReachableResult))
}
