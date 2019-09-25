package diagnostics

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

var ErrInvalidOption = errors.New("Invalid option")

type Option struct {
	Key   string
	Value string
}

func OptionsFromStrings(options []string) ([]*Option, error) {
	out := make([]*Option, len(options))
	for i, o := range options {
		opt, err := OptionFromString(o)
		if err != nil {
			return nil, errors.Wrapf(err, "%q is not a valid option", o)
		}
		out[i] = opt
	}
	return out, nil
}

func OptionFromString(option string) (*Option, error) {
	parts := strings.SplitN(option, ":", 2)
	if len(parts) == 0 {
		return nil, ErrInvalidOption
	}
	return &Option{
		Key:   parts[0],
		Value: parts[1],
	}, nil
}

func (o *Option) AsString(defaultVal string) string {
	if o == nil || o.Value == "" {
		return defaultVal
	}
	return o.Value
}

func (o *Option) AsInt(defaultVal int) int {
	if o == nil || o.Value == "" {
		return defaultVal
	}
	i, err := strconv.Atoi(o.Value)
	if err != nil {
		logrus.WithError(err).Warnf("could not parse int (%s)", o.Value)
		return defaultVal
	}
	return i
}

func (o *Option) MarshalText() (text []byte, err error) {
	if o == nil {
		return nil, ErrInvalidOption
	}
	return []byte(fmt.Sprintf("%s:%s", o.Key, o.Value)), nil
}

func (o *Option) UnmarshalText(text []byte) error {
	opt, err := OptionFromString(string(text))
	if err != nil {
		return err
	}
	*o = *opt
	return nil
}
