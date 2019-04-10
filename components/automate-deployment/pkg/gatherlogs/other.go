package gatherlogs

import (
	"io"

	"github.com/sirupsen/logrus"
)

// Other holds the fetching data
type Other struct {
	Name          string
	ExecFunc      func() ([]byte, error)
	OutputHandler func() (io.WriteCloser, error)
}

func (o *Other) execute() error {
	out, err := o.OutputHandler()
	if err != nil {
		logrus.WithError(err).Error("could not open output file")
		return err
	}
	defer out.Close() // nolint: errcheck

	output, err := o.ExecFunc()
	if err != nil {
		output = []byte(err.Error())
	}

	header := []byte(o.Name + "\n\n")
	_, err = out.Write(append(header, output...))
	return err
}
