package a1upgrade

import (
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/platform/command"
)

func TestVerifyMaintModeURLReturnsNoErrorFor503Response(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusServiceUnavailable)
		fmt.Fprintln(w, "503")
	}))
	defer ts.Close()

	client := insecureHTTPSClient()

	result := verifyMaintModeURL(client, verifyURL{
		url:            ts.URL,
		expectedReturn: 503})
	assert.Nil(t, result)
}

func TestVerifyMaintModeURLReturnsErrorForNon503Response(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprintln(w, "401")
	}))
	defer ts.Close()

	client := insecureHTTPSClient()

	result := verifyMaintModeURL(client, verifyURL{
		url:            ts.URL,
		expectedReturn: 503})
	assert.Error(t, result)
}

func TestRabbitQueueCheckSetsBasicAuth(t *testing.T) {
	var user, password string
	var basicAuthWasSet bool

	responseData := `{"messages": 0}`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		user, password, basicAuthWasSet = r.BasicAuth()
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	client := insecureHTTPSClient()

	result := testRabbitQueueIsZero(client, ts.URL, "ThePassword")

	assert.Nil(t, result)
	assert.True(t, basicAuthWasSet)
	assert.Equal(t, user, "rabbitmgmt")
	assert.Equal(t, password, "ThePassword")
}

func TestRabbitQueueCheckReturnsNilForEmptyQueue(t *testing.T) {
	responseData := `{"messages": 0}`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	client := insecureHTTPSClient()

	result := testRabbitQueueIsZero(client, ts.URL, "ThePassword")

	assert.Nil(t, result)
}

func TestRabbitQueueCheckReturnsErrorForNonEmptyQueue(t *testing.T) {
	responseData := `{"messages": 1}`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	client := insecureHTTPSClient()

	result := testRabbitQueueIsZero(client, ts.URL, "ThePassword")

	assert.Error(t, result)
}

func TestSpaceUsedByDir(t *testing.T) {
	oldDefaultCommandExecutor := defaultCommandExecutor
	defer func() { defaultCommandExecutor = oldDefaultCommandExecutor }()

	setup := func(t *testing.T) *command.MockExecutor {
		mock := command.NewMockExecutor(t)
		defaultCommandExecutor = mock
		return mock
	}

	duCmd := command.ExpectedCommand{
		Cmd:     "du",
		Args:    []string{"-s", "-k", "/some/test/path"},
		Timeout: duTimeout,
	}

	t.Run("parses du output and returns uint64", func(t *testing.T) {
		mock := setup(t)
		mock.Expect("Output", duCmd).Return("21772   /some/test/path\n", nil)
		result, err := SpaceUsedByDir("/some/test/path")
		require.NoError(t, err)
		assert.Equal(t, uint64(21772), result)
	})

	t.Run("returns error from the underlying du command", func(t *testing.T) {
		mock := setup(t)
		mock.Expect("Output", duCmd).Return("", errors.New("some du error"))
		_, err := SpaceUsedByDir("/some/test/path")
		assert.Error(t, err)
	})

	t.Run("returns error if can't parse du output", func(t *testing.T) {
		mock := setup(t)
		mock.Expect("Output", duCmd).Return("wat  /some/testpath\n", nil)
		_, err := SpaceUsedByDir("/some/test/path")
		assert.Error(t, err)
	})
}

func TestPrivKeyToPubKey(t *testing.T) {
	goodPrivKey := `-----BEGIN RSA PRIVATE KEY-----
MIIEowIBAAKCAQEApjj6uAkHmGJyn5PUnDsCJtxDCMcPdMEusswqgQkH9KLKu+/9
jtE4uXpEuB2U/E5EKGf/4merfEBEPUhqiAtZTn92tZ8Mtiqn5IdyZQgyMUkBz4sI
ROJZrCdMjOEdOFJgk/ckbbNkdPJY/Nnd5htd47Fd8BBFUfDlXt5Wx57DToc6ObgB
FOP8zAhBIH//Mr5IpMtjW7SvTRsUN9fvKyA2dGVpz3AjrUnl2UH0CfMAd3yvz4LP
8MXcXVbvF50hpOjFP4DzPKaV1ShQQlVpQphaz1Q3oMCboAIe1JX8YDYjcA5r1IXG
x/tKowozfSJN5QCFxp+4vDjP2tUGJYe0HHkaVwIDAQABAoIBAQCQA7wN6RzghSNE
aSOC/IkfvCSEHVuhz6IzHTsSMw3mgYjfDc4Eh/b6lBmn31FuzCy3SWvk7+certuw
sOZD2nlUzpC+SQANcKWoFUYijNhX5SYheBcA/4+r3RywznWvj4fHiw/Pz+aQgExj
PivIKxvMo6Z80MwOc4V3DXHoYABdLtyjYapwH2/jQ/AO31mXVzZh7rZZEM0ULz2H
kbqEKGQwHmtwYGy5FOGiF71qgPshLw/e54xZV6sha6P5c++IWDcF1qMmvddz+kpZ
ClPhZDZIgX9vhxROwYMGKY8n9HCuT6jFYj0Lnz0HxVwphtEMYTQb4OuGN5TTQnS0
bvFlbwDZAoGBANaYt7Tyi3rSYwLCsFonC6t6cuC7Hxk2hZ1dmwKH4LkE0fdkgjLT
h5b7q5VXic3L1Cx7iYTMnrg+wf0NE1apGW1G233SMIGfjjEYnn4k99o+VUBH0u7k
HUCKEck0cRF00AYIHJgr6SQ7n5uoDcRggz239e6g3oWwDb/cgf2qfygDAoGBAMZK
/Xwg7jyTjyVQM9TldioevYTAdq8cWU2vjxMD1ckT9+99hNGqzeMm4+WKw/Tqiusr
SpBB/rJ/w8Bf4rFMEoF/0Zag7v9UwwRvTQtjoqQHBGv7xBM5GzX73dsBAzVu6weY
vQMaTxyX4/tKnkQrru6TYZ1iy3Hzhl13/U4UYIYdAoGAJ+2ucMqYTTgMGB1ldsDB
GRzldw8aEDTm04rxJp1loMdW0fMvjolxNxSIrnblOxdr2QL7aWwxNBLpQU5HhN5d
zslaWaTcRaIMcPIlu07hlwf2EMnMY4qqLrIB8TACwb68XgLwXxf8fzNg1TOFrD5Q
7RDnf4kLHlDNnh9HSI2Kr0UCgYBVHW1PMcdxQxmHt7R35wLgs49AA2kuYynGGdx9
GgFTLXoYV7WCViHIJjJenkDcySxWI5/+6gUM06DP25iv5+ptodLyKCROfzCyn5/f
iqYOaGCyhUA1zCZs9q7VScr5zaGfAiXnxgw9Rsl6XR4wPtv41lRpPTX8tL5xMocN
U9vW1QKBgBns9+og/Cdz/hkcB52edxWVMRoPqqe9xSEewTVdOlgnX66MECwTCfTj
aHfwTs0RQ2Ng7MH4/l0pol+fNQDZsDX14PWkqtDpvQRKVpkV0FQboewPmRsWOYe+
9gWn+WborGTTPixSQJfY3en1C3A8ffybR6FpARNTTsVqUWPMBZP9
-----END RSA PRIVATE KEY-----
`
	t.Run("it extracts a PEM-encoded public key from a PEM-encoded private key", func(t *testing.T) {
		pubKey, err := PubKeyFromPriv(goodPrivKey)
		require.NoError(t, err)

		// Ensure we can PEM-decode it
		block, _ := pem.Decode([]byte(pubKey))
		require.NotNil(t, block)

		// Assert it is a parsable public key
		_, err = x509.ParsePKIXPublicKey(block.Bytes)
		assert.NoError(t, err)
	})

	t.Run("it returns an error if given the empty string", func(t *testing.T) {
		_, err := PubKeyFromPriv("")
		require.Error(t, err)
	})
}
