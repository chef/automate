package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckSoftwareVersion(c *fiber.Ctx) {
	query := c.Query("node_type")
	if query == "" {
		c.Next(&fiber.Error{Message: `Unsupported query or missing query. Expected values for query 'node_type' are bastion, automate, chef-infra-server, postgresql or opensearch.`, Code: fiber.StatusBadRequest})
		return
	}
	if query != constants.AUTOMATE &&
		query != constants.CHEF_INFRA_SERVER &&
		query != constants.BASTION &&
		query != constants.POSTGRESQL &&
		query != constants.OPENSEARCH {
		c.Next(&fiber.Error{Message: `The value '` + query + `' of query 'node_type' is not supported. The supported values are: bastion, automate, chef-infra-server, postgresql, opensearch.`, Code: fiber.StatusBadRequest})
		return
	}
	services, err := h.SoftwareVersionService.GetSoftwareVersionDetails(query)
	if err != nil {
		h.Logger.Error("Error while getting the Software versions Details: ", err)
		c.Next(&fiber.Error{Message: err.Error(), Code: fiber.StatusInternalServerError})
		return
	}
	c.JSON(response.BuildSuccessResponse(models.SoftwareVersionDetails{
		Passed: services.Passed,
		Checks: services.Checks,
	}))
}
