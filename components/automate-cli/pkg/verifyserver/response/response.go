package response

import "github.com/gofiber/fiber"

type ResponseBody struct {
	Result interface{}  `json:"result"`
	Error  *fiber.Error `json:"error,omitempty"`
}

func BuildSuccessResponse(response interface{}) *ResponseBody {
	return &ResponseBody{response, nil}
}

func BuildFailedResponse(err *fiber.Error) *ResponseBody {
	return &ResponseBody{nil, err}
}
