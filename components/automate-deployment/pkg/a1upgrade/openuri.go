package a1upgrade

import (
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strings"

	"github.com/pkg/errors"
)

// OpenURI opens the specified resource assumed to be a local file by default
// It supports file://, http://, and https://
func OpenURI(uri string) (io.ReadCloser, error) {
	if strings.HasPrefix(uri, "file://") {
		filepath := strings.TrimPrefix(uri, "file://")
		return os.Open(filepath)
	} else if strings.HasPrefix(uri, "http://") || strings.HasPrefix(uri, "https://") {
		resp, err := http.Get(uri)
		if err != nil {
			return nil, err
		}

		if resp.StatusCode != 200 {
			return nil, errors.Errorf("GET %s received status code %d. Expected 200", uri, resp.StatusCode)
		}
		return resp.Body, nil
	}
	return os.Open(uri)
}

// ReadURI reads the contents of the resource at the given uri
func ReadURI(uri string) ([]byte, error) {
	reader, err := OpenURI(uri)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to open uri")
	}
	defer reader.Close()
	return ioutil.ReadAll(reader)
}
