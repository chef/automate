package main

import (
	"context"
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"os/exec"

	oidc "github.com/coreos/go-oidc"
	"golang.org/x/oauth2"
)

func main() {
	automateURL, err := url.Parse("https://localhost")
	orDie(err)

	issuerURL, err := automateURL.Parse("/dex")
	orDie(err)

	// disable TLS verification
	httpClient := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
		},
	}

	// OIDC provider handling is just convenience: it'll fetch dex' discovery JSON
	// from `/dex/.well-known/openid-configuration` and extract AuthURL and
	// TokenURL.
	ctx := context.WithValue(context.Background(), oauth2.HTTPClient, httpClient)
	provider, err := oidc.NewProvider(ctx, issuerURL.String())
	orDie(err)

	oauth2Config := oauth2.Config{
		ClientID:    "automate-api",
		Endpoint:    provider.Endpoint(),
		RedirectURL: "urn:ietf:wg:oauth:2.0:oob",
		Scopes:      []string{"openid profile email offline_access groups federated:id"},
	}

	// open browser with login page
	orDie(exec.Command("open", oauth2Config.AuthCodeURL("clistate")).Start())

	// note that we could use Go to fulfill the login dance:
	// 1. figure out if we see a selection (there might only be one login method => no selection)
	//    yes => 1.1. pick the login method you want
	// 2. figure out of the login method needs us to fill in login credentials
	//    yes => 2.1. fill in credentials
	// 3. follow approval redirect
	// 4. read code from last response body
	//
	// for a concrete implementation, see the `id_token` method in
	// inspec/a2-resource-pack/libraries/automate_api_request.rb

	// alternatively: start http listener on http://localhost:0, note port that
	// was chosen, and pass that for redirect_uri; make the listener catch the
	// code response.

	// wait for the user to have logged in, and bring the code
	var code string
	fmt.Printf("code: ")
	_, err = fmt.Scanln(&code)
	orDie(err)

	token, err := oauth2Config.Exchange(ctx, code)
	orDie(err)
	idToken := token.Extra("id_token")
	fmt.Printf("id_token: %s\n", idToken)

	ingestURL, err := automateURL.Parse("/api/v0/ingest/version")
	orDie(err)
	req, err := http.NewRequest("GET", ingestURL.String(), nil)
	orDie(err)
	req.Header.Set("Authorization", fmt.Sprintf("bearer %s", idToken))

	resp, err := httpClient.Do(req)
	orDie(err)
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	orDie(err)
	fmt.Printf("response: %s\n", body)
}

// Don't do this at home
func orDie(err error) {
	if err == nil {
		return
	}
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}
