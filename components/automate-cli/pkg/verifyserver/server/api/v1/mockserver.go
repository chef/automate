package v1

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) StartMockServer(c *fiber.Ctx) error {

	reqBody := new(models.StartMockServerRequestBody)
	err := c.BodyParser(&reqBody)

	// If request body is invalid
	if err != nil {
		h.Logger.Error("Start mock-server request body parsing failed: ", err.Error())
		return fiber.NewError(fiber.StatusBadRequest, "Invalid request body")
	}

	// If port number is invalid
	if !isValidPort(reqBody.Port) {
		h.Logger.Error("Start mock-server request body contains invalid port number")
		return fiber.NewError(fiber.StatusBadRequest, "Invalid port number")
	}

	err = h.MockServersService.Start(reqBody)

	if err != nil {
		// Server is already running in the port
		if strings.Contains(err.Error(), "port unavailable") {
			h.Logger.Error("Start mock-server request body contains unavailable port: ", reqBody.Port)
			return fiber.NewError(fiber.StatusConflict, fmt.Sprintf(`Server is already running on port: %d`, reqBody.Port))
		}
		h.Logger.Error("Start mock-server request failed. Error: ", err.Error())
		return fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}

	return c.JSON(response.BuildSuccessResponse("Server started successfully"))
}

func (h *Handler) StopMockServer(c *fiber.Ctx) error {

	reqBody := new(models.StopMockServerRequestBody)
	err := c.BodyParser(&reqBody)

	// If request body is invalid
	if err != nil {
		h.Logger.Error("Stop mock server request body parsing failed: ", err.Error())
		return fiber.NewError(fiber.StatusBadRequest, "Invalid request body")
	}

	// If port number is invalid
	if !isValidPort(reqBody.Port) {
		h.Logger.Error("Start mock-server request body contains invalid port number")
		return fiber.NewError(fiber.StatusBadRequest, "Invalid port number")
	}

	err = h.MockServersService.Stop(reqBody)

	if err != nil {
		if strings.Contains(err.Error(), "no mock server found") {
			// No Server is running on given port.
			resp := fmt.Sprintf("No Mock server is running on port %d with protocol %v", reqBody.Port, reqBody.Protocol)
			h.Logger.Debug(resp)
			return fiber.NewError(fiber.StatusNotFound, resp)
		}
		h.Logger.Error("Error while stoppping server: ", err.Error())
		return fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}
	return c.JSON(response.BuildSuccessResponse("Server stopped successfully"))

}

func isValidPort(requestBodyPort int) bool {
	return requestBodyPort >= 0 && requestBodyPort <= 65535
}
