package v1

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) StopMockServer(c *fiber.Ctx) error {

	reqBody := new(models.StopMockServerRequestBody)
	err := c.BodyParser(&reqBody)

	// If request body is invalid
	if err != nil {
		h.Logger.Error("Stop mock server request body parsing failed: ", err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	// If port number is invalid
	if reqBody.Port < 0 || reqBody.Port > 65535 {
		errMsg := fmt.Sprintf("Port number %v not within range 0-65535.", reqBody.Port)
		h.Logger.Error(errMsg)
		return fiber.NewError(fiber.StatusBadRequest, errMsg)
	}

	mockServers := h.MockServersService.GetMockServers()
	if len(mockServers) < 1 {
		h.Logger.Debug("No Mock Server running")
		return c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse("No Mock Server running"))
	}

	server, updatedMockServers := getMockServer(reqBody, mockServers)
	if server != nil {
		if server.Port == reqBody.Port && server.Protocol != reqBody.Protocol {
			// Request body protocol do not match with protocol of running server on given port.
			errMsg := fmt.Sprintf("Request protocol %s does not match with running server protocol %s", reqBody.Protocol, server.Protocol)
			h.Logger.Error(errMsg)
			return fiber.NewError(fiber.StatusInternalServerError, errMsg)
		}
		if err := h.StopMockServersService.StopMockServer(server); err != nil {
			h.Logger.Error("Error while stoppping server: ", err.Error())
			return fiber.NewError(fiber.StatusInternalServerError, err.Error())
		}
		h.MockServersService.SetMockServers(updatedMockServers)
		return c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse("Server stop successfully"))
	}

	// No Server is running on given port.
	resp := fmt.Sprintf("No Mock server is running on port %d", reqBody.Port)
	h.Logger.Debug(resp)
	return c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse(resp))
}

func getMockServer(reqBody *models.StopMockServerRequestBody, mockServers []*models.Server) (*models.Server, []*models.Server) {
	var server *models.Server
	var updatedMockServers []*models.Server

	for _, s := range mockServers {
		if s.Port == reqBody.Port {
			server = s
		} else {
			updatedMockServers = append(updatedMockServers, s)
		}
	}

	return server, updatedMockServers
}
