package osnodevalidator

import "fmt"

// OSNodeValidator interface definition
type OSNodeValidator interface {
	Run() ([]string, error)
}

// OSValidator struct definition
type OSValidator struct {
}

// NewOSNodeValidator returns the new Open Search node validator
func NewOSNodeValidator() (OSNodeValidator, error) {
	return OSValidator{}, nil
}

// Run method performs all open search node validations
func (osValidator OSValidator) Run() ([]string, error) {
	fmt.Println("Running Open Search Validator")

	messages := []string{}
	var errResp error

	resp, err := test1()
	if resp != "" {
		messages = append(messages, resp)
	}
	if err != nil {
		errResp = fmt.Errorf("%s, %s", errResp.Error(), err.Error())
	}

	resp, err = test2()
	if resp != "" {
		messages = append(messages, resp)
	}
	if err != nil {
		errResp = fmt.Errorf("%s, %s", errResp.Error(), err.Error())
	}

	resp, err = test3()
	if resp != "" {
		messages = append(messages, resp)
	}
	if err != nil {
		errResp = fmt.Errorf("%s, %s", errResp.Error(), err.Error())
	}

	return messages, errResp
}

func test1() (string, error) {
	fmt.Println("Pass: OpenSearch Test1")
	return "", nil
}

func test2() (string, error) {
	fmt.Println("Pass: OpenSearch Test2")
	return "", nil
}

func test3() (string, error) {
	fmt.Println("Fail: OpenSearch Test3")

	return "Fail: Message from OpenSearch Test3", nil
}
