// Copyright © 2018 Chef Software

package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"

	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(chefServerCtlShowSecretCmd())
}

var (
	fakeSuperuserKey = `-----BEGIN RSA PRIVATE KEY-----
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
-----END RSA PRIVATE KEY-----`
	fakeWebuiPrivKey = `-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAyxeKkeNLp9rDMZZgHhdFlJ2MnAhf59KjEgxVywIwmHWukFuA
GhiUybLIZCsWqpjR9dk3TK5r6wFMQ3uC5O2ttK6yhpkT8gpxFDQtslI1dUsTSxW5
9IHkw0bt8tgrOyOIG2LcF8c2I94eeGLSCUiT/SiiO7IkF2QGJsf/vAolDQSj99zb
5iG4iZr5IMW6sGVyRCW7auF1c2ju7GGXb03zJ1BnDhoq5GmYrHmRF1TTk/So/QgQ
l6qcqmHlpWIB/wd3OAOmreQ2vAqEmYV09d1TWpLWcnWlUW8nsoyjwCp1i2n/6Top
gxzmqOSgLaz7kSP8Q+7WhLXTKRxn4hP8k9tbuwIDAQABAoIBAEy9rILL3H6kzzUh
mVhFeulZTxR2paa3lR7wwkpfnXU+oqOxr4os16wPfmE7VulHra60UY0MK4M1jlfB
iAhxNz4a9SKfHUZMY8Y6oTnMjp6vq66nmwilPIf9eVRWEp/aIUgX5wqlH/QVrnXX
AX64ihMTE5pzCeg8x4ZH9H5dZvqvpdAHtQNXCDvgggEIgNYaVz2h0CSUOR2ZuoVq
Nf155tNVk2FAsc10kx11szwrFaYZvz/BZYPEfBudcOC4njWduPpaPPy1JY5i4NvT
Qg8FXI20INslP6s0gDV3lg1HdEr+yiZs2SE2vNrmBcP/msanF2UMzhXBgWRM4hrm
H9gq2CECgYEA6uriVSLbSYKVqPOy7TzJYwwKtxRod84m3Vhq6qtJXBdqW9TPaLjn
lT6k6FV/zB3xRGzJBhgw0TGKiI0sWiJ33O2GGzYhQbTR7l62WIxhjBxYrum0foyq
Uqv1uZhckng2DJV6vlP+Fi1aW3R8oaNsWMsiPWFGzQZa6Wg9L7WHvMUCgYEA3VF6
/bzS8lN4Sf+6QItFQE1j25/TCMqRazFrCngEYVXRYcg9SbxKzgZShcFlCJtGsn5L
JL5OwBmDQH/G2QueS+5pSjpVj+PcrD3Tjq5L5iQ185sNCqVMrJCT5qxRCLhIcsw2
LnXfR2Omtet07D5/IgCiDsDw/zYqv0YwquVpPn8CgYAuArs5uY4Bo9X0NWcCGDnJ
cH5YxWKAtJvgFoU6WToRE3T9b30AIdcZ4B8kcjqJnAO5RyVIUEyfbHiZrizVqpKX
bt4pTjzNRcEoUtycE0K30oV8HMIBV0O01YFXTIQ1G95aMJzN0l2dslxUNUiD0CJU
cdTO+NKJLYl41bXGNmPTkQKBgFkH+i4w07tgELfUBB0K6aTgfRdQyBh1ROKWUrU3
aqutVjm7z8vlz0HqETB1iCtaK6qKW7hARnXIzaAYxulxXxC0q+6MYPctdwxjIsP3
IdlHH90Ltsddj+arkpPtJfwYBi0IMZOnH1cco2fzfGm9Cm+nSYod29UO+UtQ9SsK
drgJAoGACIE0LKoSlT/uds7e4HTVyHIj5U+XT+8zNJvXjgwAaMFY/L8RWyjLYLVO
gJIkP73ibliDOcY0Oi/6mRpEiosJvkI7jvqYtmy5C6quaFcai469eyvS6ctOvKPH
MEvewSgVoY46c+Vbm2GQ75lr6BW0l+MmPm/f8QiOYlMPToDUHns=
-----END RSA PRIVATE KEY-----`
	fakeWebuiPubKey = `-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyxeKkeNLp9rDMZZgHhdF
lJ2MnAhf59KjEgxVywIwmHWukFuAGhiUybLIZCsWqpjR9dk3TK5r6wFMQ3uC5O2t
tK6yhpkT8gpxFDQtslI1dUsTSxW59IHkw0bt8tgrOyOIG2LcF8c2I94eeGLSCUiT
/SiiO7IkF2QGJsf/vAolDQSj99zb5iG4iZr5IMW6sGVyRCW7auF1c2ju7GGXb03z
J1BnDhoq5GmYrHmRF1TTk/So/QgQl6qcqmHlpWIB/wd3OAOmreQ2vAqEmYV09d1T
WpLWcnWlUW8nsoyjwCp1i2n/6TopgxzmqOSgLaz7kSP8Q+7WhLXTKRxn4hP8k9tb
uwIDAQAB
-----END PUBLIC KEY-----`
)

// automateCtl/stopCmd represents the automateCtl/stop command
func chefServerCtlShowSecretCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "show-secret",
		Short: "test double of `chef-server-ctl show-secret`",
		Long: `chef-server-ctl show-secret will print hard-coded versions of

the credentials we expect to export from Chef Server.  Requesting any
other credentials will result in an error.

To test failure cases set the FAILURE environment to 602:

FAILURE=602 chef-server-ctl show-secret GROUP_NAME SECRET_NAME

`,
		Run: chefServerCtlShowSecret,
	}
}

func chefServerCtlShowSecret(cmd *cobra.Command, args []string) {
	// The automate-ctl double will make a network call here to shutdown the stub
	// a1 server; we don't have a stub chef server so we don't need that part here.
	if a1upgrade.Failure(a1upgrade.ChefServerShowSecretFail) {
		fmt.Println("chef-server-ctl show-secret FAILURE (simulated)")
		os.Exit(1)
	}

	groupName := args[0]
	secretName := args[1]

	switch groupName {
	case "chef-server":
		switch secretName {
		case "superuser_key":
			fmt.Println(fakeSuperuserKey)
		case "webui_key":
			fmt.Println(fakeWebuiPrivKey)
		case "webui_pub_key":
			fmt.Println(fakeWebuiPubKey)
		default:
			fmt.Printf("chef-server-ctl show-secret FAILURE (no mock output for %s %s\n", groupName, secretName)
			os.Exit(1)
		}
	default:
		fmt.Printf("chef-server-ctl show-secret FAILURE (no mock output for %s %s\n", groupName, secretName)
		os.Exit(1)
	}

	os.Exit(0)
}
