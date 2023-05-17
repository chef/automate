package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckSoftwareVersion(c *fiber.Ctx) {
	if c.Query("node_type") == "" {
		c.Next(&fiber.Error{Message: "The empty query is not supported. The Supported query's are: postgres, opensearch, bastion, automate, chef-server", Code: fiber.StatusBadRequest})
		return
	}
	services, err := h.SoftwareVersionService.GetSoftwareVersionDetails(c.Query("node_type"))
	if err != nil {
		h.Logger.Error("Error while getting the Software versions = ", err)
		c.Next(&fiber.Error{Message: err.Error(), Code: fiber.StatusBadRequest})
		return
	}
	c.JSON(response.BuildSuccessResponse(models.SoftwareVersionDetails{
		Passed: services.Passed,
		Checks: services.Checks,
	}))
}
