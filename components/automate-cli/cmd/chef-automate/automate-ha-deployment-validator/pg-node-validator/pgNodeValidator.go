package pgnodevalidator

import "fmt"

// PGNodeValidator interface definition
type PGNodeValidator interface {
	Run() ([]string, error)
}

// PGValidator struct definition
type PGValidator struct {
}

// NewPGNodeValidator returns the new PG node validator
func NewPGNodeValidator() (PGNodeValidator, error) {
	return PGValidator{}, nil
}

// Run method performs all postgres node validations
func (pgValidator PGValidator) Run() ([]string, error) {
	fmt.Println("Running Postgres Validator")

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
	fmt.Println("Pass: PG Test1")
	return "", nil
}

func test2() (string, error) {
	fmt.Println("Pass: PG Test2")
	return "", nil
}

func test3() (string, error) {
	fmt.Println("Fail: PG Test3")

	return "Fail: Message from PG Test3", nil
}
