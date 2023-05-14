package v1

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) StopMockServer(c *fiber.Ctx) {

	reqBody := new(models.StopMockServerRequestBody)
	err := c.BodyParser(&reqBody)
	if err != nil {
		errMsg := fiber.Error{
			Code:    fiber.StatusBadRequest,
			Message: fmt.Sprintf("Stop mock server request body parsing failed: %v", err.Error()),
		}
		h.Logger.Error(fmt.Errorf(errMsg.Message))
		c.Next(&errMsg)
		return
	}

	if reqBody.Port < 0 || reqBody.Port > 65535 {
		errMsg := fiber.Error{
			Code:    fiber.StatusBadRequest,
			Message: fmt.Sprintf("Port number %v not within range 0-65535.", reqBody.Port),
		}
		h.Logger.Error(fmt.Errorf(errMsg.Message))
		c.Next(&errMsg)
		return
	}

	mockServers := h.MockServersService.GetMockServers()
	if len(mockServers) < 1 {
		h.Logger.Debug("No Mock Server running")
		c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse("No Mock Server running"))
		return
	}

	server, updatedMockServerServices := getMockServer(reqBody, mockServers)
	if server != nil {
		if server.Port == reqBody.Port && server.Protocol != reqBody.Protocol {
			// Request body protocol do not match with protocol of running server on given port.
			errMsg := fiber.Error{
				Code:    fiber.StatusInternalServerError,
				Message: fmt.Sprintf("Request protocol %s does not match with running server protocol %s", reqBody.Protocol, server.Protocol),
			}
			h.Logger.Error(fmt.Errorf(errMsg.Message))
			c.Next(&errMsg)
			return
		}
		if err := h.StopMockServerService.StopMockServer(server); err != nil {
			errMsg := fiber.Error{
				Code:    fiber.StatusInternalServerError,
				Message: fmt.Sprintf("Error while stoppping server: %v", err.Error()),
			}
			h.Logger.Error(err.Error())
			c.Next(&errMsg)
			return
		}
		h.MockServersService.SetMockServers(updatedMockServerServices)
		c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse("Server stop successfully"))
		return
	}

	// No Server is running on given port.
	resp := fmt.Sprintf("No Mock server is running on port %d", reqBody.Port)
	h.Logger.Debug(resp)
	c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse(resp))
}

func getMockServer(reqBody *models.StopMockServerRequestBody, mockServers []*models.Server) (*models.Server, []*models.Server) {
	var server *models.Server
	var updatedMockServerServices []*models.Server

	for _, s := range mockServers {
		if s.Port == reqBody.Port {
			server = s
		} else {
			updatedMockServerServices = append(updatedMockServerServices, s)
		}
	}

	return server, updatedMockServerServices
}
