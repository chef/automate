package validation

import (
	"reflect"
	"strings"
)

type (
	// Rules represents rules for fields with name and rules
	Rules map[string][]string

	// Options represent configuration option for validator
	Options struct {
		Target          string
		Request         interface{}
		RequiredDefault bool
		Rules           Rules
	}

	// Validator is a validator with options
	Validator struct {
		Opts Options // Opts contains all the options for validator
	}

	// FieldViolation represents custom bad request error field violation error option.
	FieldViolation struct {
		Field       string
		Description string
	}
)

// New returns a new validator object with provided options
func New(opts Options) *Validator {
	return &Validator{Opts: opts}
}

// Validate validates the validation object raise the error if any
func (v *Validator) Validate() error {
	reqVal := reflect.ValueOf(v.Opts.Request)
	typeOfS := reqVal.Type()

	if v.Opts.RequiredDefault {
		for i := 0; i < reqVal.NumField(); i++ {
			fv := RequiredField(getFieldValue(reqVal, i), getFieldTag(typeOfS, i), v.Opts.Target)
			if fv != nil {
				return NewPlainError(fv)
			}
		}
	}

	for field, rules := range v.Opts.Rules {
		for _, rule := range rules {
			switch rule {
			case "required":
				fv := RequiredField(getFieldValue(reqVal, field), getFieldTag(typeOfS, field), v.Opts.Target)
				if fv != nil {
					return NewPlainError(fv)
				}
			case "ipv4":
				return nil //TODO: Dev debt
			case "regex":
				return nil //TODO: Dev debt
			}
		}
	}

	return nil
}

func getFieldTag(rType reflect.Type, id interface{}) string {
	var field reflect.StructField
	switch id.(type) {
	case int:
		field = rType.Field(id.(int))
	default:
		field, _ = rType.FieldByName(id.(string))
	}

	return strings.Split(field.Tag.Get("json"), ",")[0]
}

func getFieldValue(value reflect.Value, id interface{}) string {
	switch id.(type) {
	case int:
		return value.Field(id.(int)).String()
	default:
		return value.FieldByName(id.(string)).String()
	}

}
