package habapi

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestWait(t *testing.T) {
	oldWaitInterval := defaultWaitInterval
	defaultWaitInterval = time.Duration(0)
	defer func() { defaultWaitInterval = oldWaitInterval }()

	responseDataUp := `[
	{
	 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev" },
	 "process": { "pid": 27847, "state": "up", "state_entered": 1518709870 }
	}
	]`

	responseDataDown := `[
	{
	 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev" },
	 "process": { "pid": 27847, "state": "down", "state_entered": 1518709870 }
	}
	]`

	responseDataUpUpper := `[
	{
	 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"},
	 "process": { "pid": 27847, "state": "Up", "state_entered": 1518709870 }
	}
	]`

	responseDataDownUpper := `[
	{
	 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"},
	 "process": { "pid": 27847, "state": "Down", "state_entered": 1518709870 }
	}
	]`

	responseDataDuplicates := `[
	{
	 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"},
	 "process": { "pid": 27847, "state": "down", "state_entered": 1518709870 }
	},
	{
	 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"},
	 "process": { "pid": 27848, "state": "up", "state_entered": 1518709870 }
	}
	]`

	responseDataUnloaded := `[]`
	pkg := habpkg.New("core", "acme")
	testTimeout := 25 * time.Millisecond

	var handler func(http.ResponseWriter)
	var handlerMut sync.Mutex

	setHandler := func(h func(http.ResponseWriter)) {
		handlerMut.Lock()
		handler = h
		handlerMut.Unlock()
	}

	getHandler := func() func(http.ResponseWriter) {
		handlerMut.Lock()
		defer handlerMut.Unlock()
		return handler
	}

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		getHandler()(w)
	}))
	defer ts.Close()
	c := New(ts.URL)

	t.Run("WaitForDown returns no error if all the services go down in time", func(t *testing.T) {
		callCount := 0
		setHandler(func(w http.ResponseWriter) {
			switch callCount {
			case 0:
				fmt.Fprintln(w, responseDataUp)
			default:
				fmt.Fprintln(w, responseDataDown)
			}
			callCount = callCount + 1
		})
		// NOTE(ssd) 2018-10-25: This first test uses a higher
		// timeout since it is hopefully the only test that is
		// opening a new TCP connection.
		err := WaitForDown(c, []habpkg.VersionedPackage{&pkg}, 100*time.Millisecond)
		require.NoError(t, err)
	})

	t.Run("WaitForDown returns an error out if the service never goes down", func(t *testing.T) {
		setHandler(func(w http.ResponseWriter) {
			fmt.Fprintln(w, responseDataUp)
		})
		err := WaitForDown(c, []habpkg.VersionedPackage{&pkg}, testTimeout)
		require.Error(t, err)
	})

	t.Run("WaitForDown returns no error if all the services go down in time with old upper case process state", func(t *testing.T) {
		callCount := 0
		setHandler(func(w http.ResponseWriter) {
			switch callCount {
			case 0:
				fmt.Fprintln(w, responseDataUpUpper)
			default:
				fmt.Fprintln(w, responseDataDownUpper)
			}
			callCount = callCount + 1
		})
		err := WaitForDown(c, []habpkg.VersionedPackage{&pkg}, testTimeout*2)
		require.NoError(t, err)

	})

	t.Run("WaitForDown considers unloaded services as down", func(t *testing.T) {
		callCount := 0
		setHandler(func(w http.ResponseWriter) {
			switch callCount {
			case 0:
				fmt.Fprintln(w, responseDataUp)
			default:
				fmt.Fprintln(w, responseDataUnloaded)
			}
			callCount = callCount + 1
		})
		pkg := habpkg.New("core", "acme")
		err := WaitForDown(c, []habpkg.VersionedPackage{&pkg}, testTimeout*2)
		require.NoError(t, err)
	})

	t.Run("WaitForDown isn't confused by duplicate services", func(t *testing.T) {
		setHandler(func(w http.ResponseWriter) {
			fmt.Fprintln(w, responseDataDuplicates)
		})
		err := WaitForDown(c, []habpkg.VersionedPackage{&pkg}, testTimeout)
		require.Error(t, err)
	})

	t.Run("WaitForDown returns an error out if the service never goes down", func(t *testing.T) {
		setHandler(func(w http.ResponseWriter) {
			fmt.Fprintln(w, responseDataUp)
		})
		err := WaitForDown(c, []habpkg.VersionedPackage{&pkg}, testTimeout)
		require.Error(t, err)
	})

	t.Run("WaitForUp returns no error if all the services are up in time", func(t *testing.T) {
		callCount := 0
		setHandler(func(w http.ResponseWriter) {
			switch callCount {
			case 0:
				fmt.Fprintln(w, responseDataDown)
			default:
				fmt.Fprintln(w, responseDataUp)
			}
			callCount = callCount + 1
		})
		err := WaitForUp(c, []habpkg.VersionedPackage{&pkg}, testTimeout*2)
		require.NoError(t, err)

	})

	t.Run("WaitForUp returns no error if all the services are up in time with old upper case process state", func(t *testing.T) {
		callCount := 0
		setHandler(func(w http.ResponseWriter) {
			switch callCount {
			case 0:
				fmt.Fprintln(w, responseDataDownUpper)
			default:
				fmt.Fprintln(w, responseDataUpUpper)
			}
			callCount = callCount + 1
		})
		err := WaitForUp(c, []habpkg.VersionedPackage{&pkg}, testTimeout*2)
		require.NoError(t, err)

	})

	t.Run("WaitForUp considers unloaded services as down", func(t *testing.T) {
		setHandler(func(w http.ResponseWriter) {
			fmt.Fprintln(w, responseDataUnloaded)
		})
		err := WaitForUp(c, []habpkg.VersionedPackage{&pkg}, testTimeout)
		require.Error(t, err)
	})

	t.Run("WaitForUp isn't confused by duplicate services", func(t *testing.T) {
		setHandler(func(w http.ResponseWriter) {
			fmt.Fprintln(w, responseDataDuplicates)
		})
		err := WaitForUp(c, []habpkg.VersionedPackage{&pkg}, testTimeout)
		require.Error(t, err)
	})
}
