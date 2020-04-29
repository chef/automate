package validation

import (
	"fmt"
	"reflect"
	"regexp"
	"strings"
)

// EmptyOrWhitespaceOnlyRE is a regex that checks for blank or whitespace only strings
var EmptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

// RequiredField verifies that the given field is not empty
func RequiredField(field, fieldName, resourceName string) error {
	if EmptyOrWhitespaceOnlyRE.MatchString(field) {
		return fmt.Errorf("a %s %s is required and must contain at least one non-whitespace character", resourceName, fieldName)
	}
	return nil
}

type (
	// Rules msg
	Rules map[string][]string

	// Options msg
	Options struct {
		Target          string
		Request         interface{}
		RequiredDefault bool
		Rules           Rules
	}

	// Validator msg
	Validator struct {
		Opts Options // Opts contains all the options for validator
	}
)

// New func
func New(opts Options) *Validator {
	return &Validator{Opts: opts}
}

// Validate msg
func (v *Validator) Validate() error {
	reqVal := reflect.ValueOf(v.Opts.Request)
	typeOfS := reqVal.Type()
	if v.Opts.RequiredDefault {
		for i := 0; i < reqVal.NumField(); i++ {
			err1 := RequiredField(reqVal.Field(i).String(), strings.Split(typeOfS.Field(i).Tag.Get("json"), ",")[0], v.Opts.Target)
			if err1 != nil {
				return err1
			}
		}
	}

	for field, rules := range v.Opts.Rules {
		for _, rule := range rules {
			name, _ := typeOfS.FieldByName(field)
			switch rule {
			case "required":
				err1 := RequiredField(reqVal.FieldByName(field).String(), strings.Split(name.Tag.Get("json"), ",")[0], v.Opts.Target)
				if err1 != nil {
					return err1
				}
			case "ipv4":
				return nil
			case "regex":
				return nil
			}
		}
	}

	return nil
}
