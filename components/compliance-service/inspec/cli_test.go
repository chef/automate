package inspec

import (
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestInspecErrReturnsCorrectErr(t *testing.T) {
	target := &TargetConfig{
		TargetBaseConfig: TargetBaseConfig{Hostname: "test"},
	}
	blankErr := errors.New("")

	// winrm auth error
	stdErr := "/hab/pkgs/chef/inspec/4.18.51/20191211220937/lib/gems/winrm-2.3.3/lib/winrm/http/response_handler.rb:59:in raise_if_auth_error': WinRM::WinRMAuthorizationError (WinRM::WinRMAuthorizationError)\n\tfrom /hab/pkgs/chef/inspec/4.18.51/20191211220937/lib/gems/winrm-2.3.3/lib/winrm/http/response_handler.rb:51:in raise_if_error'\n\tfrom /hab/pkgs/chef/inspec/4.18.51/20191211220937/lib/gems/winrm-2.3.3/lib/winrm/http/response_handler.rb:34:in parse_to_xml'\n\tfrom /hab/pkgs/chef/inspec/4.18.51"
	err := getInspecError("", stdErr, blankErr, target, time.Duration(90))
	assert.Equal(t, &Error{Type: "authentication failed", Message: "Authentication failed for test\n\nWinRM::WinRMAuthorizationError"}, err)

	// some random error
	stdErr = "some random error from inspec that we haven't checked for"
	err = getInspecError("", stdErr, blankErr, target, time.Duration(90))
	assert.Equal(t, &Error{Type: "unknown error", Message: "Unknown inspec error for test: \n\nSTDERR: some random error from inspec that we haven't checked for"}, err)

	// conn refused error
	stdErr = "some thing will be here and then Errno::ECONNREFUSED and then more words"
	err = getInspecError("", stdErr, blankErr, target, time.Duration(90))
	assert.Equal(t, &Error{Type: "connection refused", Message: "Failed to connect to test, connection refused.\n\nErrno::ECONNREFUSED"}, err)

	// ssh auth error
	stdErr = "/hab/pkgs/chef/inspec/4.18.51/20191211220937/lib/gems/ more words here Net::SSH::AuthenticationFailed other details"
	err = getInspecError("", stdErr, blankErr, target, time.Duration(90))
	assert.Equal(t, &Error{Type: "authentication failed", Message: "Authentication failed for test\n\nNet::SSH::AuthenticationFailed"}, err)

}
