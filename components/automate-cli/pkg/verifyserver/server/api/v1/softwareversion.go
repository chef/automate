package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) CheckSoftwareVersion(c *fiber.Ctx) error {
	query := c.Query("node_type")
	if query == "" {
		return fiber.NewError(fiber.StatusBadRequest, `Unsupported query or missing query. Expected values for query 'node_type' are bastion, automate, chef-infra-server, postgresql or opensearch.`)
	}
	if query != constants.AUTOMATE &&
		query != constants.CHEF_INFRA_SERVER &&
		query != constants.BASTION &&
		query != constants.POSTGRESQL &&
		query != constants.OPENSEARCH {
		return fiber.NewError(fiber.StatusBadRequest, `The value '`+query+`' of query 'node_type' is not supported. The supported values are: bastion, automate, chef-infra-server, postgresql, opensearch.`)
	}
	services, err := h.SoftwareVersionService.GetSoftwareVersionDetails(query)
	if err != nil {
		h.Logger.Error("Error while getting the Software versions Details: ", err)
		return fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}

	return c.JSON(response.BuildSuccessResponse(models.SoftwareVersionDetails{
		Passed: services.Passed,
		Checks: services.Checks,
	}))
}
