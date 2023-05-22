package v1

import (
	"errors"
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

func (h *Handler) StartMockServer(c *fiber.Ctx) {

	reqBody := new(models.StartMockServerRequestBody)
	err := c.BodyParser(&reqBody)

	// If request body is invalid
	if err != nil {
		errString := fmt.Sprintf("start mock-server request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid request body"})
		return
	}

	// If port number is invalid
	err = validatePortRange(reqBody.Port, h)
	if err != nil {
		h.Logger.Error(err)
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid port number"})
		return
	}

	// Server is already running in the port
	if err = validatePortAlreadyInUse(reqBody.Port, h); err != nil {
		h.Logger.Error(err)
		c.Next(&fiber.Error{Code: http.StatusConflict, Message: fmt.Sprintf(`"%s" server is already running on port %d`, reqBody.Protocol, reqBody.Port)})
		return
	}

	err = h.MockServersService.StartMockServer(reqBody)

	if err != nil {
		errString := fmt.Sprintf("start mock-server request failed. Error: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Next(&fiber.Error{Code: http.StatusInternalServerError, Message: err.Error()})
		return
	}
}

func validatePortRange(requestBodyPort int, h *Handler) error {
	if requestBodyPort < 0 || requestBodyPort > 65535 {
		errString := "start mock-server request body contains invalid port number"
		fmt.Print(errString)
		return errors.New(errString)
	}
	return nil
}

func validatePortAlreadyInUse(port int, h *Handler) error {
	h.Logger.Infof("Handle server value: %v\n", h.MockServersService.GetMockServers())
	for _, s := range h.MockServersService.GetMockServers() {
		if s.Port == port {
			errString := fmt.Sprintf("start mock-server request body contains unavailable port: %v", port)
			return errors.New(errString)
		}
	}
	return nil
}
