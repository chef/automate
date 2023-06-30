package certificatevalidation_test

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/certificatevalidation"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	ROOT_CERTIFICATE = `-----BEGIN CERTIFICATE-----
MIIEdTCCA12gAwIBAgIJAKcOSkw0grd/MA0GCSqGSIb3DQEBCwUAMGgxCzAJBgNV
BAYTAlVTMSUwIwYDVQQKExxTdGFyZmllbGQgVGVjaG5vbG9naWVzLCBJbmMuMTIw
MAYDVQQLEylTdGFyZmllbGQgQ2xhc3MgMiBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0
eTAeFw0wOTA5MDIwMDAwMDBaFw0zNDA2MjgxNzM5MTZaMIGYMQswCQYDVQQGEwJV
UzEQMA4GA1UECBMHQXJpem9uYTETMBEGA1UEBxMKU2NvdHRzZGFsZTElMCMGA1UE
ChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjE7MDkGA1UEAxMyU3RhcmZp
ZWxkIFNlcnZpY2VzIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IC0gRzIwggEi
MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDVDDrEKvlO4vW+GZdfjohTsR8/
y8+fIBNtKTrID30892t2OGPZNmCom15cAICyL1l/9of5JUOG52kbUpqQ4XHj2C0N
Tm/2yEnZtvMaVq4rtnQU68/7JuMauh2WLmo7WJSJR1b/JaCTcFOD2oR0FMNnngRo
Ot+OQFodSk7PQ5E751bWAHDLUu57fa4657wx+UX2wmDPE1kCK4DMNEffud6QZW0C
zyyRpqbn3oUYSXxmTqM6bam17jQuug0DuDPfR+uxa40l2ZvOgdFFRjKWcIfeAg5J
Q4W2bHO7ZOphQazJ1FTfhy/HIrImzJ9ZVGif/L4qL8RVHHVAYBeFAlU5i38FAgMB
AAGjgfAwge0wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYwHQYDVR0O
BBYEFJxfAN+qAdcwKziIorhtSpzyEZGDMB8GA1UdIwQYMBaAFL9ft9HO3R+G9FtV
rNzXEMIOqYjnME8GCCsGAQUFBwEBBEMwQTAcBggrBgEFBQcwAYYQaHR0cDovL28u
c3MyLnVzLzAhBggrBgEFBQcwAoYVaHR0cDovL3guc3MyLnVzL3guY2VyMCYGA1Ud
HwQfMB0wG6AZoBeGFWh0dHA6Ly9zLnNzMi51cy9yLmNybDARBgNVHSAECjAIMAYG
BFUdIAAwDQYJKoZIhvcNAQELBQADggEBACMd44pXyn3pF3lM8R5V/cxTbj5HD9/G
VfKyBDbtgB9TxF00KGu+x1X8Z+rLP3+QsjPNG1gQggL4+C/1E2DUBc7xgQjB3ad1
l08YuW3e95ORCLp+QCztweq7dp4zBncdDQh/U90bZKuCJ/Fp1U1ervShw3WnWEQt
8jxwmKy6abaVd38PMV4s/KCHOkdp8Hlf9BRUpJVeEXgSYCfOn8J3/yNTd126/+pZ
59vPr5KW7ySaNRB6nJHGDn2Z9j8Z3/VyVOEVqQdZe4O/Ui5GjLIAZHYcSNPYeehu
VsyuLAOQ1xk4meTKCRlb/weWsKh/NEnfVqn3sF/tM+2MR7cwA130A4w=
-----END CERTIFICATE-----`

	PRIVATE_KEY = `-----BEGIN PRIVATE KEY-----
MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQCyoSEPPhH7uy9M
/sodHNH9WIAFZSSHBEabNMnUI+IBmh1/4rwNIlvCJ53467+JslMPbtsjZYlLllrU
7pPbtSGKcWSgn6ExJO/oSAUVzk07JQGAR6aZlFxL9IoGur0ejl/dyHzbggnAmplL
P9gFAznWNX1EgRUBhyCQpY6AlY+gJqod7lmaj/ANwndantJz9bFKubNfSAHvp6Fl
2TQ9psziZ8Rgu2MWOk+BA5BplV/6+h4kCc/YNR3Il8L5RIbJi4WgIV3g2rlCqnE6
MhFIwI/Eli4DnwhrqPoWCcZbN0S3hHqojFRwKBqhfTQfvzdM1yC1Dcaam7lPuMp/
lfEPCzm7AgMBAAECggEBAIV9hIcvi6DjseWIQuKetw7tJD7j5vg/acRzHqE6b5ti
4MjBWGc4VfoyQQvUob71VL0jywAY0RF7u0WFRVnbcsVTGDAxGvTu9ZpQt/wjv/OD
JCwJX6dGB/N7O+rrgoljPKExQp2IAGG/Ju/lNjoIvyAluQeIr3oE0YFHN8c//aVf
/+jZJbpMIhcaTMQlvmpIrTCHaFQP153qsGmsO5IpkM4VqUr2pEPNnYSVr2y5/RUA
wmA5zDVwUlWLwL+98hSR6Dt/X2rOkK6qrQIw/YjN2eyvshdWSMBkLfOWxUpozRAQ
F55gVJLN6A2SazN7rV/48Lyla29zErF/MAb5d24Ii4ECgYEA22Bzv274MxmDkWjs
tLmYCZpo8WhV7BDxt6+UnuSPHGk1ih4NO5dL34/HVEJRQub30I+OdL2uje5NI6HC
36fBdPjHUwKcx02OhRTAX/3itEvAWwjcNe3EJMcUw0LxHBhtIaWgTrJYJL5UKcKD
cfctwzDnNUn3AZbuKhVFVKbJIvUCgYEA0HM/0Nke/cCpwr4qMw3gwJAtKt0k34U2
IXjmInHnlcErlP+w1aX6Y83XhSb3SZN6BtJOdrTYphF/x60nPHytZU29wmOQLgUw
2m63hwG2SORrnetplegfosDshM2ZlcyvmlYtvg1AI5MRqTxUcCKyOgbWJ357ZFDo
5MtyUsVs2+8CgYEA1t/if99xvJuBAyyCrYIcKUvU/p/ziQYPvJt+NyQoCOwERQSq
cXQAHTn3Q9CxR5ZzpRT3s+EW2G5RoFURS4BFPQ1wLx6f8ZdFwZRR2bQUGgUYpBJJ
n4kEVgGplJg9Egfa3Nod6vdE3riON1kO5tH1QPrlfIy2JBhxr0amYgP7fwECgYA4
/noBYYw+t3HzJEmngfDk1cbzxFlzoBmn10WdP2WhpZqD99Fi6SnoJfz6lCfi/ybL
PK9ld5cQ+RHmB6sFt9U1MKdkwhJG8vZjkwgtKsOrdIEUQayx5zDXMEa7HPgUZE3T
NiL1puvVjj2y9yLjMilnAj2FuOiGCjVTcWkFxqM4EwKBgQCW4xepcop8JipiDlUr
wFuo+xBs9J0cRwnoQYsSsJEn4L2HDnoA8NFnd5yNwLJOx/S8ZAWxu7nHAzset+Sc
1EPO3etxY/VYrAXxYwpCW16GlZzv+DwwAukByNyMe9YujOf9Xh3a1JYrFmTKkYRL
+EKJmv/2qCOY9WFUQN42dJ65ZA==
-----END PRIVATE KEY-----`

	NODE_CERTIFICATE = `-----BEGIN CERTIFICATE-----
MIIDajCCAlKgAwIBAgIJALREK+d4fUqNMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNjAxMTA0MTM2WhcNMjYwNTMxMTA0MTM2WjBjMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxETAPBgNVBAMMCGNoZWZub2RlMIIBIjANBgkqhkiG9w0B
AQEFAAOCAQ8AMIIBCgKCAQEAsqEhDz4R+7svTP7KHRzR/ViABWUkhwRGmzTJ1CPi
AZodf+K8DSJbwied+Ou/ibJTD27bI2WJS5Za1O6T27UhinFkoJ+hMSTv6EgFFc5N
OyUBgEemmZRcS/SKBrq9Ho5f3ch824IJwJqZSz/YBQM51jV9RIEVAYcgkKWOgJWP
oCaqHe5Zmo/wDcJ3Wp7Sc/WxSrmzX0gB76ehZdk0PabM4mfEYLtjFjpPgQOQaZVf
+voeJAnP2DUdyJfC+USGyYuFoCFd4Nq5QqpxOjIRSMCPxJYuA58Ia6j6FgnGWzdE
t4R6qIxUcCgaoX00H783TNcgtQ3Gmpu5T7jKf5XxDws5uwIDAQABoyEwHzAdBgNV
HSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwEwDQYJKoZIhvcNAQELBQADggEBAJKH
vZHX9u2/OeT03JynES8UNG+2G8KC6fqiZyQqMHb/XfGeFKkgtnZrc3mZO6F7iQ0K
X5uFDuwVit2wo17u+l/riI/GvW/co3dY2c6NZJfLyTjuOe3uId5B/MvLrv1SUyi/
Md6f3R+MoWnu6nodwlYhLk69je8lCmxYkS7qQPCqX/yU2222D41W74o12RddzJ6D
YkZWPUAHSSjkGCtC7nu9fTh0gpuZ0njSf+oke3TidndFutCU61g7NyoBDH2nmMEn
Wt9OIzM5m3FAZLIzjqZhIuRMAIsQYSkvrgLGORKXXsk8v0uL7sZVMhUxeY24TvAD
eFw0aM7Eys16crD0U4Y=
-----END CERTIFICATE-----`

	ADMIN_PRIVATE_KEY = `-----BEGIN PRIVATE KEY-----
MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQDWmTeu7jtCBCrh
1c7qgvzF0xamhJsqL5D5v85Y1IjbfQ7qf8V6cj35rJXhA/4I/s5HByc8Xg85LZ+1
IDsExqQ3btaRB+GTj5oSezq4CSPWGqqK2AP7xJPFEE5Y5LIRsnScFC9CKsKpVkLW
xICW5n9TmKeh3/n7WFM6bxO0EsW7NXEHza6KFa1mbxNdmIn2HHtiA8zBajDTe4uC
iuRnhhPU2hDP7MO3Yt7wLgKpgXSJ8h9DdzlNcR+ZU3Hi4ghFUT44NjJswgQKBW56
N/hjDUMIgXJHr+cK8UQH/TrdG2vpKiTWx0BpOADRtGAh9vIBQ5/5C1pT6TzTU0OF
29VphJyNAgMBAAECggEAB66QabipcmGHz3HB6G8r8UDvLKLOkyIExIvzLQbep0zA
vfA493sjcM6WxulHrLbCV5SP+P88+EUD1I4EztT1h+51y9dGYevgDg1POstI9x8Z
P8Mf0WFmEMXWqCP3o96JghTO5EV3lmw6FEoQeFZj9HAggBDud+rff9X+FJIfdeYR
DyaWVW8vrvLI1f5b3lW+s/xWmiRl+rqtPPKXmtQ2UtNvUC5v5C5I+e7f+9jjBXUR
+iaQ5B5RVLpSwo9gjEDiGCd4ks5tLTdZgPaIyCfCVrNYoTYqNcvT2sa15xov9z61
RKeXz/pz090lT6xbfblrLQNSzKdbbRI+QT8cJiT2QQKBgQDuvnNeEH4LZJV/T1Cn
u7qD8Hn9NBafDekQiqcHWPWbtXf4YRpr7dQ3qaI5bFfqt5+mxx7GUgFfwdKsUFMf
baYGvRQD1BEWZwQF4qKhroE6H1qvu4zmKpoq77qi/f5jv2C7Bc7Y0BVE9o31mIMG
JAnqrgximSvQTyE/bo8qgJSQKQKBgQDmG/8lCsbeqhCZbU4Cc9Kvc9NLdk06Igk1
Y7iXSDNfISfPJRUeGoGmmNcW2tuH0eAsL3bjvVLIdRzfOpdM1qPCK71+AcaJSJha
zwNchcinFly9P6oo0ba6F8MQlK6zw8sacfC3FfE2sVv51XAVijGI8T5HZBR+lXkE
zSp4I/XlxQKBgQDpprRry8I/zinNVcd+0OhCNjh9SNwWv9tn1/qN409EG36VNBtS
o4i9DSR6BIGqchkqdNe2ig1UNPHEuDwRlxMUpzmOYZ5ziVBTnjFCAPHx+zuanvfP
TsRg3wuOdvvfjS3S+2UrpcIbIx1dzev1N5II/luap4NMtBS66q3sj7hVuQKBgQCi
9Xi/dt5XmgCfZzUzr2qpLlGYw3L1lF0JHlH/cPZQjF5w6EYIR6VBokPQv7VwfkVP
hV2g8tbJd4a23/t+QxM901QjBIbhejPLO4eZ4nSzP7b1nzyWI85QA7v05hbC4AnZ
9I8QWc21ee01BN6XooJqBM6iKFILQzetGs7UvkLKMQKBgQCmZu2Z43evQd5PGrx8
KVZkxSDgphwErbAMCnYZNbCNXVuUI4qjrM9h/r9JkA8+DV7R3pKGll3imWHfc3cV
oqHm5FrIstadbhI5ZvVys5CvvAEkuV2nfAncXImH9EfFPmnH2IbOcTZTZITju98L
aiImBJe8IEDUia3JVN6IA9BbIQ==
-----END PRIVATE KEY-----`

	ADMIN_CERTIFICATE = `-----BEGIN CERTIFICATE-----
MIIDazCCAlOgAwIBAgIJALREK+d4fUqMMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNjAxMTA0MTM2WhcNMjYwNTMxMTA0MTM2WjBkMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEjAQBgNVBAMMCWNoZWZhZG1pbjCCASIwDQYJKoZIhvcN
AQEBBQADggEPADCCAQoCggEBANaZN67uO0IEKuHVzuqC/MXTFqaEmyovkPm/zljU
iNt9Dup/xXpyPfmsleED/gj+zkcHJzxeDzktn7UgOwTGpDdu1pEH4ZOPmhJ7OrgJ
I9YaqorYA/vEk8UQTljkshGydJwUL0IqwqlWQtbEgJbmf1OYp6Hf+ftYUzpvE7QS
xbs1cQfNrooVrWZvE12YifYce2IDzMFqMNN7i4KK5GeGE9TaEM/sw7di3vAuAqmB
dInyH0N3OU1xH5lTceLiCEVRPjg2MmzCBAoFbno3+GMNQwiBckev5wrxRAf9Ot0b
a+kqJNbHQGk4ANG0YCH28gFDn/kLWlPpPNNTQ4Xb1WmEnI0CAwEAAaMhMB8wHQYD
VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMA0GCSqGSIb3DQEBCwUAA4IBAQBk
gJ0snZhv742txGbJvzhEtNu6wJe7qE5B1Nw9cZynL5h6bLuPukUXh6C4C50woM0r
YXgen0fYi6a46/HQjRypKwjIzY0Qbxqr2Ck9oNVobxcw1Wf9/F//7PrHh+gaP1BH
KY+y2Lxz+ylhMSBBouf8m6zeWzWEN292qAxavTYFX5OSnqMdq4/scFrhB6KqcmnL
SMsVrOjekCjDO+4s8ZSHzqCaWbp6dfsI1ZJxjXUfRnwqv+R4SAYVb5A0WJyFGDIi
nvFIH/UBywRuAjgns0T7geRwQwBuZzYA3SxWY2MjaX+NMePOD0Cr0i0IYmQK8FzJ
68hKDbWi4DG1XdT6p/I8
-----END CERTIFICATE-----`

	EXPIRED_NODE_CERTIFICATE = `-----BEGIN CERTIFICATE-----
MIIFRjCCAy4CCQCPGzD2HWF1ezANBgkqhkiG9w0BAQsFADBlMQswCQYDVQQGEwJJ
TjEMMAoGA1UECAwDTlNXMRIwEAYDVQQHDAlCZW5nYWx1cnUxFTATBgNVBAoMDEdv
TGludXhDbG91ZDEMMAoGA1UECwwDT3JnMQ8wDQYDVQQDDAZSb290Q0EwHhcNMjMw
NjAyMDg1MTE0WhcNMjMwNjAzMDg1MTE0WjBlMQswCQYDVQQGEwJJTjEMMAoGA1UE
CAwDTlNXMRIwEAYDVQQHDAlCZW5nYWx1cnUxFTATBgNVBAoMDEdvTGludXhDbG91
ZDEMMAoGA1UECwwDT3JnMQ8wDQYDVQQDDAZSb290Q0EwggIiMA0GCSqGSIb3DQEB
AQUAA4ICDwAwggIKAoICAQDpPYG29VzGYXjP7A/bD/IVSoJGYq+VJqb612PUtLsV
IbTiWyWbMslsH3JwCyp2YxnS9JX5h9WzKm7fXkbwBUhFOf6A8P2bWVYRLde4dpeT
kKPs7fjqo7UChZVRdrlCkPoopHB4APqwPCb3oBoh9nakJjQurTsh9P5qUpAvc/4B
hTxr8G9Vg5czGmFr6nDYxKYcrrctBIbnSRWOY360N1BY3uvcbUBnygvddirdv7Wl
bdn7esXOJCQRAT2f/77Q40ufhCQmLSu4lNknQotj8vIM7t3lhfYurSZxDRgItEM3
XPGVle9zVdOaVvyUJV9dsNIuPLIxmwZ0GPgqkzxtDILZ5pHGH3LYLUlG2m8Vg8ox
5OSHkL/ATcGU4qOpKIhqXF9mT5k5GIOEc2PTxa9q9b1zgUOOw5fLJZfOO9ljgNA9
NSSCiMdGxDSzJc0cx6Rum1TXTYA4ygZyXx1fHAZqFMHgVf0ZGjd56gBJzPky/c6y
21Pk9Q9BQla501dxeX/1M5CM87r4pSe3hy7CZSDfLUdndqLo9qSfHZft7emABAR2
mNPEEuEiNLRVvx1bMByMtpDSCnB+1HCFCGY4fH2v+7oQjL1Z817vAhFBAJX2xTkz
xJeXkot9JBaE82aJ9p209yej0XOIodNcqCtKcjxPONI2Jx7aEXq5YIt7Y8erpfh1
pQIDAQABMA0GCSqGSIb3DQEBCwUAA4ICAQAQK/iprdH0ceazChXIrDO/C8XXB0nL
/ri2DiUEzE/9HPUk9JSLSeIswFgLEWHXuTA+scd6r6+tJbPLWoc5soY/nbVLfHWV
Avhy+0tfES8n4m2HPwZuwrCqmE5/taJ13u3WweGA+M5b17OteO3vRU12NkeGfEtj
JpfSWCgnWJxdwT6l7uG8reZxM2CBA5DitJDuM2FTcl0OBA5en2lgI7K/694MHla0
UF4aQyMGhjFw25XkjgJ8+SHp4sdVAudQGkfxcKCKItCj4d2eHoJdrh+pUcAzHqXE
Ya65WJhGivybvrkhOyXG7Ne2q8Fxc0hYmBHKJq33mm2XoEhcGI61o+Hl64C92Yik
V5whEXa/u7zYSRbBsmtttuY5eeD08iB/Z6pgdA2IvpxXeCwsPNW6iPraSCkt0+So
W/emgCPa2fVAAr6HwHG2EpnQQW2KJjiaIKh8najCng6ZKJB8xGOAoSRpBoznGgwG
hZKbCMdcU3KZ3G+FbD0Voyum+gOKzxFTlHN9Vf7OuCFQPOIjXVf66MDcwlquXBf0
hB89adTtx88hhBsunPb/D/sSLPkB1BQD9qM+Pt6BM1g8by+ZrnYNnZ3SzRhV//Pw
M8hhfcvF8pXjfrIEaU2oXFV+NfH6suewJtO5Mzzhm2WhcYzXeIcOVr699eHakSzr
LUV1BxbSsLDq7A==
-----END CERTIFICATE-----`

	CERT_WITH_INVALID_FORMAT_HASH_ALGO = `-----BEGIN CERTIFICATE-----
MIIFTjCCAzYCCQD4TjazM2YSczANBgkqhkiG9w0BAQwFADBpMQswCQYDVQQGEwJJ
bjELMAkGA1UECAwCa3QxCzAJBgNVBAcMAmJuMREwDwYDVQQKDAhwcm9ncmVzczEL
MAkGA1UECwwCaXQxIDAeBgkqhkiG9w0BCQEWEXRlc3RAcHJvZ3Jlc3MuY29tMB4X
DTIzMDYwNTEwMTIzN1oXDTI0MDYwNDEwMTIzN1owaTELMAkGA1UEBhMCSW4xCzAJ
BgNVBAgMAmt0MQswCQYDVQQHDAJibjERMA8GA1UECgwIcHJvZ3Jlc3MxCzAJBgNV
BAsMAml0MSAwHgYJKoZIhvcNAQkBFhF0ZXN0QHByb2dyZXNzLmNvbTCCAiIwDQYJ
KoZIhvcNAQEBBQADggIPADCCAgoCggIBAMjJ14Lxn1HVO0qX3FZ/8ogJr+9f4GLa
jiGU9t8tuLa4d96YFsLKzTr+B2LAhs3F8KZ5VcjBoilAxbQk0HYGGNUVu2umt2y4
ZMeH6K6ng+NrnfCBzvJPsWY7knP7ZJHJ3IGiTIqKacGaQ5Fv+LuGAybTgcPQ2Fmc
yGobxNi3NTuKYKpvzZr+LRhREpu9t6/stBgiX4p00TsGms7xBU+VStV34qdZyvmc
rwh7bO7BFuXkh8PulBP2CmY66TiyqQZPMOxGmSvHVBIK+r+JVg6aW2edJ0srPPWH
ZkOFmDC8lE0js6TOJnDP1IvDL3wvtZ/YcZs4pDHGNbGfkcWleRdmtp6yBkZtkRuO
oEEMYCHiaRV7wfZjf+6tevGuy3TM4OS1IATMoW5djuH9Q1iU2Y2oaufPsqoL8wGM
CsfpkLCpQRHW4/5Ibjj2kctWkAZt+1hANCgWewS447PPfJgi/UF46oKomCu3il1U
NDH/gJOkijD1B7NywCPd1m3wSUgopsrmvIvgKeLYDqv3RdbD9qvX9jpqylws57KC
XhIPURtVzVB7VV0Enx7ffQANMdIdm5/xSL2COnRXCmmd6XRK1bVQlLPEg1R+k/dq
gG5S050J8/hh/lvnGHrA5RK/jnC12akht499GOrpl4J4TsMK+i0VgovdctcMmdsO
HlU5/wK31IX5AgMBAAEwDQYJKoZIhvcNAQEMBQADggIBAKSrk3CQETKdtu9QF5jZ
FEkpxaHl5cUkbAaOAHKsdR1hDM82+26i1UUkzVkWDudCcdYLJwlwmySpj9LLlVPz
tmVcr0GmBjKFcEMsJeyDnq6GHv41zY/mVDUpIfFJUgBE9IsthbnzjP+U73W5DX39
Ysr3hsCzZqpbK6EOVnUTb/gCeDdnYrH+97gmYaAWEiaocgEOokbgcpfVvS59iQEK
aCdyuI3cqEnQUqj5PzfGs9JtptDjtAHZwjpAib5w9Q50KoT9BWMaQP+lE5KLGmwv
CquIRGjImRLOUHowndWRGVHkLBYBh7GFjSXsatnBxK6ywn4/HVeMzvMac1ACGSS4
H6k4D7pmRZD56I3OoibhfioKJXDStVoGGq8HNuREz/r6i19EQbmD8Ux7GPmF5aGN
Ny/+Dby6JnsUArXcpxUb4SxCFJ5dZQJ+1QqxkAEvzC43IRW7uLPS5VfEi+bO4aFB
Yg+BODPhBXLlBUzbfKc5rr22epvwcK+SYYG5MuK/bCpkOQNv2rHVRqkRbMWsDmnD
u7FiLi9VMX03u/uqIZOVOYBRHya5d6oYnfpuhQVzzDt1ni3jcdfylZHN6oSlmf+B
qHVUqFT3SjZjuRZimtyhnGN+raexURFriLT2blXtqiMWtlYkhNUkw8zywH4OAHIm
GGIp8aM8p4osdTQXgrXkYACk
-----END CERTIFICATE-----`

	INVALID_ROOT_CERTIFICATE = `-----BEGIN CERTIFICATE-----
MIIBmTCCAQKgAwIBAgIJANsPlZp1G0g6MAoGCCqGSM49BAMCMIGzMQswCQYDVQQG
EwJVUzETMBEGA1UECAwKQ2FsaWZvcm5pYTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNj
eTELMAkGA1UECgwCSVQxCzAJBgNVBAsMAklUMRowGAYDVQQDDBF3d3cuZXhhbXBs
ZS5jb20wHhcNMjEwNDEyMTM1MTM5WhcNMjIwNDEyMTM1MTM5WjCBszELMAkGA1UE
BhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExFjAUBgNVBAcMDVNhbiBGcmFuY2lz
Y3kxCzAJBgNVBAoMAklUMQswCQYDVQQLDAJJVDEaMBgGA1UEAwwRd3d3LmV4YW1w
bGUuY29tMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEzZM6tLoBdzjsCOEl8W1t
KxTIBW56+54vlRXO3lgJirWW+un1ViJZr3Qq5+/1CijU1Rj8Q+4rThpx9Y3j7N1i
5qNjMGEwHQYDVR0OBBYEFBZnu9HidO1EFT1DnjN8Uy+AXVuxMAoGCCqGSM49BAMA
A0gAMEUCIQCZQbUd8DmzwP1UcThdNyFZOLcN1YdPAnGKv1riM3n8jwIgP1IJKNo8
t0WcQFnqRRtXOVDCZuBE4jgFJgLP1p7p7qI=
-----END CERTIFICATE-----`

	INVALID_BLOCK = `INVALID`

	INVALID_KEY_FORMAT = `-----BEGIN RSA PRIVATE KEY-----
MIICXAIBAAKBgQCjcGqTkOq0CR3rTx0ZSQSIdTrDrFAYl29611xN8aVgMQIWtDB/
lD0W5TpKPuU9iaiG/sSn/VYt6EzN7Sr332jj7cyl2WrrHI6ujRswNy4HojMuqtfa
b5FFDpRmCuvl35fge18OvoQTJELhhJ1EvJ5KUeZiuJ3u3YyMnxxXzLuKbQIDAQAB
AoGAPrNDz7TKtaLBvaIuMaMXgBopHyQd3jFKbT/tg2Fu5kYm3PrnmCoQfZYXFKCo
ZUFIS/G1FBVWWGpD/MQ9tbYZkKpwuH+t2rGndMnLXiTC296/s9uix7gsjnT4Naci
5N6EN9pVUBwQmGrYUTHFc58ThtelSiPARX7LSU2ibtJSv8ECQQDWBRrrAYmbCUN7
ra0DFT6SppaDtvvuKtb+mUeKbg0B8U4y4wCIK5GH8EyQSwUWcXnNBO05rlUPbifs
DLv/u82lAkEAw39sTJ0KmJJyaChqvqAJ8guulKlgucQJ0Et9ppZyet9iVwNKX/aW
9UlwGBMQdafQ36nd1QMEA8AbAw4D+hw/KQJBANJbHDUGQtk2hrSmZNoV5HXB9Uiq
7v4N71k5ER8XwgM5yVGs2tX8dMM3RhnBEtQXXs9LW1uJZSOQcv7JGXNnhN0CQBZe
nzrJAWxh3XtznHtBfsHWelyCYRIAj4rpCHCmaGUM6IjCVKFUawOYKp5mmAyObkUZ
f8ue87emJLEdynC1CLkCQHduNjP1hemAGWrd6v8BHhE3kKtcK6KHsPvJR5dOfzbd
HAqVePERhISfN6cwZt5p8B3/JUwSR8el66DF7Jm57BM=
-----END RSA PRIVATE KEY-----`
)

func TestValidateCertificateService(t *testing.T) {
	vc := certificatevalidation.NewValidateCertificateService(logger.NewTestLogger())
	assert.NotNil(t, vc)
}

func TestCertificateValidation(t *testing.T) {
	vc := certificatevalidation.NewValidateCertificateService(logger.NewTestLogger())
	assert.NotNil(t, vc)
	tests := []struct {
		TestName        string
		ReqBody         models.CertificateCheckRequest
		ExpectedResBody models.CertificateCheckResponse
	}{
		{
			TestName: "All Certificates are valid",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  ROOT_CERTIFICATE,
				PrivateKey:       PRIVATE_KEY,
				NodeCertificate:  NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: ADMIN_CERTIFICATE,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_EXPIRY_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_ALGORITHM_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			TestName: "Root Certificate is about to expire, not in x509 v3 format, uses incorrect Hash Algo and Node Certificate is expired.",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  CERT_WITH_INVALID_FORMAT_HASH_ALGO,
				PrivateKey:       PRIVATE_KEY,
				NodeCertificate:  EXPIRED_NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: ADMIN_CERTIFICATE,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprint(fmt.Sprintf(constants.CERTIFICATE_EXPIRY_ERROR_MESSAGE, constants.NODE) + "; " + fmt.Sprintf(constants.CERTIFICATE_INVALID_EXPIRY_MESSAGE, constants.ROOT)),
						ResolutionMsg: constants.CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_FORMAT_ERROR_MESSAGE, fmt.Sprintf("%s, %s", constants.ROOT, constants.NODE)),
						ResolutionMsg: constants.CERTIFICATE_FORMAT_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE, constants.ROOT),
					},
				},
			},
		},
		{
			TestName: "Node Certificate is about to expire, not in x509 v3 format, uses incorrect Hash Algo",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate: ROOT_CERTIFICATE,
				PrivateKey:      PRIVATE_KEY,
				NodeCertificate: CERT_WITH_INVALID_FORMAT_HASH_ALGO,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_INVALID_EXPIRY_MESSAGE, constants.NODE),
						ResolutionMsg: constants.CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_FORMAT_ERROR_MESSAGE, constants.NODE),
						ResolutionMsg: constants.CERTIFICATE_FORMAT_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_ERROR_MESSAGE, constants.NODE),
						ResolutionMsg: fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE, constants.NODE),
					},
				},
			},
		},
		{
			TestName: "Admin Certificate is about to expire, not in x509 v3 format, uses incorrect Hash Algo",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  ROOT_CERTIFICATE,
				PrivateKey:       PRIVATE_KEY,
				NodeCertificate:  NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: CERT_WITH_INVALID_FORMAT_HASH_ALGO,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_INVALID_EXPIRY_MESSAGE, constants.ADMIN),
						ResolutionMsg: constants.CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_FORMAT_ERROR_MESSAGE, constants.ADMIN),
						ResolutionMsg: constants.CERTIFICATE_FORMAT_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_ERROR_MESSAGE, constants.ADMIN),
						ResolutionMsg: fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE, constants.ADMIN),
					},
				},
			},
		},
		{
			TestName: "Invalid Block Root Certificate",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  INVALID_BLOCK,
				PrivateKey:       PRIVATE_KEY,
				NodeCertificate:  NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: ADMIN_CERTIFICATE,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_EXPIRY_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: constants.CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_FORMAT_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: constants.CERTIFICATE_FORMAT_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE, constants.ROOT),
					},
				},
			},
		},
		{
			TestName: "Failed to Parse Root Certificate",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  INVALID_ROOT_CERTIFICATE,
				PrivateKey:       PRIVATE_KEY,
				NodeCertificate:  NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: ADMIN_CERTIFICATE,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_EXPIRY_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: constants.CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_FORMAT_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: constants.CERTIFICATE_FORMAT_RESOLUTION_MESSAGE,
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_ERROR_MESSAGE, constants.ROOT),
						ResolutionMsg: fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE, constants.ROOT),
					},
				},
			},
		},
		{
			TestName: "Invalid Block for Private Key",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  ROOT_CERTIFICATE,
				PrivateKey:       INVALID_BLOCK,
				NodeCertificate:  NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: ADMIN_CERTIFICATE,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_EXPIRY_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.KEY_FORMAT_ERROR_MESSAGE, constants.NODE_KEY),
						ResolutionMsg: fmt.Sprintf(constants.KEY_FORMAT_RESOLUTION_MESSAGE, constants.NODE_KEY),
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_ALGORITHM_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},

		{
			TestName: "There is no root certificate",
			ReqBody: models.CertificateCheckRequest{
				RootCertificate:  "",
				PrivateKey:       PRIVATE_KEY,
				NodeCertificate:  NODE_CERTIFICATE,
				AdminPrivateKey:  ADMIN_PRIVATE_KEY,
				AdminCertificate: ADMIN_CERTIFICATE,
			},
			ExpectedResBody: models.CertificateCheckResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.CERTIFICATE_EXPIRY_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_EXPIRY_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.KEY_FORMAT_TITLE,
						Passed:        true,
						SuccessMsg:    constants.KEY_FORMAT_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_ALGORITHM_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_ALGORITHM_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			res := vc.CertificateValidation(e.ReqBody)
			assert.Equal(t, e.ExpectedResBody, res)
		})
	}
}
