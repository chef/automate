package bastionnodevalidator

import "fmt"

// BastionNodeValidator interface definition
type BastionNodeValidator interface {
	Run() ([]string, error)
}

// BastionValidator struct definition
type BastionValidator struct {
}

// NewBastionNodeValidator returns the new bastion node validator
func NewBastionNodeValidator() (BastionNodeValidator, error) {
	return BastionValidator{}, nil
}

// Run method performs all bastion node validations
func (bValidator BastionValidator) Run() ([]string, error) {
	fmt.Println("Running Bastion Validator")
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
	fmt.Println("Pass: Bastion Test1")
	return "", nil
}

func test2() (string, error) {
	fmt.Println("Pass: Bastion Test2")
	return "", nil
}

func test3() (string, error) {
	fmt.Println("Fail: Bastion Test3")

	return "Fail: Message from Bastion Test3", nil
}
