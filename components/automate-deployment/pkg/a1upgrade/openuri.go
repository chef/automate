package a1upgrade

import (
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strings"

	"github.com/pkg/errors"
)

// openURI opens the specified resource assumed to be a local file by default
// It supports file://, http://, and https://
func openURI(uri string) (io.ReadCloser, error) {
	if strings.HasPrefix(uri, "file://") {
		filepath := strings.TrimPrefix(uri, "file://")
		return os.Open(filepath)
	} else if strings.HasPrefix(uri, "http://") || strings.HasPrefix(uri, "https://") {
		// Note: the resp.Body io.ReadCloser is returned, and closed by the sole
		// caller of this function: ReadURI.
		resp, err := http.Get(uri) // nolint: bodyclose
		if err != nil {
			return nil, err
		}

		if resp.StatusCode != 200 {
			// Note: we don't return resp.Body here, so let's close it.
			if err := resp.Body.Close(); err != nil {
				return nil, errors.Errorf(
					"GET %s received status code %d (!= 200). Also failed to Close() response body",
					uri, resp.StatusCode)
			}
			return nil, errors.Errorf("GET %s received status code %d. Expected 200", uri, resp.StatusCode)
		}
		return resp.Body, nil
	}
	return os.Open(uri)
}

// ReadURI reads the contents of the resource at the given uri
func ReadURI(uri string) ([]byte, error) {
	reader, err := openURI(uri)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to open uri")
	}
	defer reader.Close() // nolint: errcheck
	return ioutil.ReadAll(reader)
}
