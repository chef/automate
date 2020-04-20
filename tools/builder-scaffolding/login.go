package main

import (
	"bufio"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"os"
	"regexp"
	"strings"
	"syscall"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"golang.org/x/crypto/ssh/terminal"
)

var loginCmd = &cobra.Command{
	Use:  "login",
	RunE: runLogin,
}

var loginCmdOpts = struct {
	AutomateURL string
	Username    string
	Password    string
}{}

func init() {
	loginCmd.PersistentFlags().StringVarP(&loginCmdOpts.AutomateURL,
		"url", "u", "https://localhost/", "")
	loginCmd.PersistentFlags().StringVar(&loginCmdOpts.Username,
		"user", "", "")
	loginCmd.PersistentFlags().StringVar(&loginCmdOpts.Password,
		"password", "", "")
}

func doLogin(username, password string) (string, error) {
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{
			InsecureSkipVerify: true,
		},
	}
	cookieJar, err := cookiejar.New(nil)
	if err != nil {
		return "", err
	}
	client := &http.Client{
		Transport: tr,
		Jar:       cookieJar,
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			return http.ErrUseLastResponse
		},
	}

	baseURL, err := url.Parse(loginCmdOpts.AutomateURL)
	if err != nil {
		return "", err
	}
	newSessionURL, err := endpoint(
		loginCmdOpts.AutomateURL,
		"/session/new",
		"client_id=automate-builder-api&redirect_uri=%2Fbldr&response_type=code&scope=openid%20profile%20email&nonce=0",
	)
	if err != nil {
		return "", err
	}

	respNewSession, err := client.Get(newSessionURL)
	if err != nil {
		return "", err
	}

	dexAuthLocation, err := respNewSession.Location()
	if err != nil {
		return "", errors.Wrap(err, "Failed to get dex auth location")
	}
	fixURL(baseURL, dexAuthLocation)

	respDexAuth, err := client.Get(dexAuthLocation.String())
	if err != nil {
		return "", errors.Wrap(err, "Failed to call dex auth location")
	}

	var req string
	if loc, err := respDexAuth.Location(); err == nil {
		req = loc.Query().Get("req")
	} else {
		body, err := ioutil.ReadAll(respDexAuth.Body)
		if err != nil {
			return "", err
		}
		re := regexp.MustCompile(`/dex/auth/local\?req=([^"]+)`)
		matches := re.FindStringSubmatch(string(body))
		if len(matches) < 2 {
			return "", errors.New("could not find req id")
		}
		req = matches[1]
	}

	localLoginURL, err := endpoint(loginCmdOpts.AutomateURL,
		"/dex/auth/local", fmt.Sprintf("req=%s", req))
	if err != nil {
		return "", err
	}
	form := url.Values{}
	form.Set("login", username)
	form.Set("password", password)
	loginResp, err := client.PostForm(localLoginURL, form)
	if err != nil {
		return "", err
	}

	if loginResp.StatusCode != 303 {
		return "", errors.Errorf("received unexpected login error code: got %d expected %d",
			loginResp.StatusCode, 303)
	}

	approvalLoc, _ := loginResp.Location()
	fixURL(baseURL, approvalLoc)

	approvalResp, err := client.Get(approvalLoc.String())
	if err != nil {
		return "", err
	}

	if approvalResp.StatusCode != 303 {
		return "", errors.Errorf("received unexpected approval error code: got %d expected %d",
			approvalResp.StatusCode, 303)
	}

	callbackLoc, _ := approvalResp.Location()
	fixURL(baseURL, callbackLoc)

	callbackResp, err := client.Get(callbackLoc.String())
	if err != nil {
		return "", err
	}

	if callbackResp.StatusCode != 303 {
		return "", errors.Errorf("received unexpected callback error code: got %d expected %d",
			callbackResp.StatusCode, 303)
	}

	bldrLoc, _ := callbackResp.Location()
	code := bldrLoc.Query().Get("code")
	if code == "" {
		return "", errors.New("Could not find code")
	}

	bldrAuthenticateURL, err := endpoint(
		loginCmdOpts.AutomateURL,
		fmt.Sprintf("/bldr/v1/authenticate/%s", code),
		"")
	if err != nil {
		return "", err
	}
	bldrAuthenticateResp, err := client.Get(bldrAuthenticateURL)
	if err != nil {
		return "", err
	}

	if bldrAuthenticateResp.StatusCode != 200 {
		return "", errors.Errorf(
			"received unexpected bldr auth error code: got %d expected %d",
			bldrAuthenticateResp.StatusCode,
			200,
		)
	}

	token := struct {
		Token string `json:"token"`
	}{}

	if err := json.NewDecoder(bldrAuthenticateResp.Body).Decode(&token); err != nil {
		return "", err
	}

	return token.Token, nil
}

func runLogin(c *cobra.Command, args []string) error {
	username, password, err := getUserCredentials(loginCmdOpts.Username, loginCmdOpts.Password)
	if err != nil {
		return err
	}

	token, err := doLogin(username, password)
	if err != nil {
		return err
	}

	fmt.Printf("%s\n", token)
	return nil
}

func fixURL(baseURL *url.URL, u *url.URL) {
	u.Scheme = baseURL.Scheme
	u.Host = baseURL.Host
}

func endpoint(automateURL string, path string, params string) (string, error) {
	u, err := url.Parse(automateURL)
	if err != nil {
		return "", err
	}

	u.Path = path

	return fmt.Sprintf("%s?%s", u.String(), params), nil
}

// Shamelessly taken from
// https://stackoverflow.com/questions/2137357/getpasswd-functionality-in-go
func getUserCredentials(username, password string) (string, string, error) {
	var err error
	reader := bufio.NewReader(os.Stdin)

	if username == "" {
		fmt.Print("Enter Username: ")
		username, err = reader.ReadString('\n')
		if err != nil {
			return "", "", err
		}
	}

	if password == "" {
		fmt.Print("Enter Password: ")
		bytePassword, err := terminal.ReadPassword(int(syscall.Stdin))
		if err != nil {
			return "", "", err
		}
		password = string(bytePassword)
	}

	return strings.TrimSpace(username), strings.TrimSpace(password), nil
}
