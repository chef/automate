package a1upgrade

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestOpenURI(t *testing.T) {
	t.Run("simple file", func(t *testing.T) {
		f, err := ioutil.TempFile("", "openuri-test")
		require.NoError(t, err)
		defer os.Remove(f.Name())
		defer f.Close()

		_, err = f.WriteString("test-string")
		require.NoError(t, err)
		err = f.Close()
		require.NoError(t, err)

		reader, err := OpenURI(f.Name())
		require.NoError(t, err)
		defer reader.Close()

		data, err := ioutil.ReadAll(reader)
		require.NoError(t, err)
		assert.Equal(t, []byte("test-string"), data)
	})
	t.Run("file://", func(t *testing.T) {
		f, err := ioutil.TempFile("", "openuri-test")
		require.NoError(t, err)
		defer os.Remove(f.Name())
		defer f.Close()

		_, err = f.WriteString("test-string")
		require.NoError(t, err)
		err = f.Close()
		require.NoError(t, err)

		reader, err := OpenURI(fmt.Sprintf("file://%s", f.Name()))
		require.NoError(t, err)
		defer reader.Close()

		data, err := ioutil.ReadAll(reader)
		require.NoError(t, err)
		assert.Equal(t, []byte("test-string"), data)
	})
	t.Run("http://", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("test-string"))
		}))
		defer ts.Close()
		reader, err := OpenURI(ts.URL)
		require.NoError(t, err)
		defer reader.Close()

		data, err := ioutil.ReadAll(reader)
		require.NoError(t, err)
		assert.Equal(t, []byte("test-string"), data)
	})
}
