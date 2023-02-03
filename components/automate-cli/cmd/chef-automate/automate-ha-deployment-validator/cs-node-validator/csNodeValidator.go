package csnodevalidator

import "fmt"

// CSNodeValidator interface definition
type CSNodeValidator interface {
	Run() ([]string, error)
}

// CSValidator struct definition
type CSValidator struct {
}

// NewCSNodeValidator returns the new CS node validator
func NewCSNodeValidator() (CSNodeValidator, error) {
	return CSValidator{}, nil
}

// Run method performs all chef server node validations
func (csValidator CSValidator) Run() ([]string, error) {
	fmt.Println("Running Chef Server Validator")

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
	fmt.Println("Pass: CS Test1")
	return "", nil
}

func test2() (string, error) {
	fmt.Println("Pass: CS Test2")
	return "", nil
}

func test3() (string, error) {
	fmt.Println("Fail: CS Test3")

	return "Fail: Message from CS Test3", nil
}
