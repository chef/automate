package automatenodevalidator

import "fmt"

// AutomateNodeValidator interface definition
type AutomateNodeValidator interface {
	Run() ([]string, error)
}

// AutomateValidator struct definition
type AutomateValidator struct {
}

// NewAutomateNodeValidator returns the new Automate node validator
func NewAutomateNodeValidator() (AutomateNodeValidator, error) {
	return AutomateValidator{}, nil
}

// Run method performs all automate node validations
func (aValidator AutomateValidator) Run() ([]string, error) {
	fmt.Println("Running Automate Validator")

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
	fmt.Println("Pass: Automate Test1")
	return "", nil
}

func test2() (string, error) {
	fmt.Println("Pass: Automate Test2")
	return "", nil
}

func test3() (string, error) {
	fmt.Println("Fail: Automate Test3")

	return "Fail: Message from Automate Test3", nil
}
