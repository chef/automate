package response

import "github.com/gofiber/fiber"

const (
	SUCCESS = "SUCCESS"
	FAILED  = "FAILED"
)

type ResponseBody struct {
	Status string       `json:"status"`
	Result interface{}  `json:"result"`
	Error  *fiber.Error `json:"error,omitempty"`
}

func BuildSuccessResponse(response interface{}) *ResponseBody {
	return &ResponseBody{SUCCESS, response, nil}
}

func BuildFailedResponse(err *fiber.Error) *ResponseBody {
	return &ResponseBody{FAILED, nil, err}
}
