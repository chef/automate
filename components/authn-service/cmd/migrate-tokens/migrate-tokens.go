package main

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"flag"
	"fmt"
	"net/http"
)

var (
	authToken = flag.String("auth-token", "unset", "auth token")
	client    = &http.Client{}
)

const (
	tokensURL  = "https://localhost/apis/iam/v2/tokens"    // nolint
	secretsURL = "https://localhost/api/v0/secrets/search" // nolint
)

type Secrets struct {
	Secrets []Secret
}

type Secret struct {
	Name string
	Tags []Tag
}

type Tag struct {
	Key   string
	Value string
}

type token struct {
	ID     string `json:"id"`
	Name   string `json:"name"`
	Value  string `json:"value"`
	Active bool   `json:"active"`
}

func main() {
	http.DefaultTransport.(*http.Transport).TLSClientConfig = &tls.Config{InsecureSkipVerify: true} // nolint

	flag.Parse()

	fmt.Printf("migrating tokens from secrets service to authn service using:\n")
	fmt.Printf("\tsecrets URL: %s\n", secretsURL)
	fmt.Printf("\ttokens URL: %s\n", tokensURL)
	fmt.Printf("\tapi token: %s\n\n", *authToken)

	tokens := getTokensFromSecrets()
	fmt.Printf("tokens read from secrets service...\n")
	for _, t := range tokens {
		fmt.Printf("\treading token '%s' from secrets\n", t.Name)
	}
	fmt.Printf("\ttotal tokens read from secrets service: %d\n\n", len(tokens))

	successfulWrites := writeTokensToClients(tokens)
	if successfulWrites != len(tokens) {
		fmt.Printf("WARNING! only migrated %d out of %d tokens", successfulWrites, len(tokens))
	}
}

func getTokensFromSecrets() []*token {
	var emptyBody = []byte(`{}`)
	req, err := http.NewRequest("POST", secretsURL, bytes.NewBuffer(emptyBody))
	if err != nil {
		panic(err)
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("api-token", *authToken)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close() //nolint

	decoder := json.NewDecoder(resp.Body)
	var secrets Secrets
	err = decoder.Decode(&secrets)
	if err != nil {
		panic(err)
	}
	var tokens []*token
	for _, secret := range secrets.Secrets {
		token := clientTokenFromSecret(&secret)
		if token != nil {
			tokens = append(tokens, token)
		}
	}
	return tokens
}

func writeTokensToClients(tokens []*token) int {
	fmt.Printf("writing tokens to authn service...\n")
	successfulWrites := 0
	for _, t := range tokens {
		requestBody, err := json.Marshal(*t)
		if err != nil {
			panic(err)
		}
		req, err := http.NewRequest("POST", tokensURL, bytes.NewBuffer(requestBody))
		if err != nil {
			panic(err)
		}

		req.Header.Set("Content-Type", "application/json")
		req.Header.Set("api-token", *authToken)

		resp, err := client.Do(req)
		if err != nil {
			fmt.Printf("\tFAILED to write token %s, error: %v\n", t.Name, err)
		}
		defer resp.Body.Close() //nolint
		if resp.StatusCode != 200 {
			fmt.Printf("\tFAILED to write token %s, status code: %v\n", t.Name, resp.Status)
		} else {
			fmt.Printf("\twrote token %q successfully\n", t.Name)
			successfulWrites++
		}
	}
	fmt.Printf("\ttotal tokens written to authn service: %d\n", successfulWrites)
	return successfulWrites
}

func clientTokenFromSecret(secret *Secret) *token {
	if !isClientToken(secret) {
		return nil
	}

	tok, err := valueForKey(&secret.Tags, "token")
	if err != nil {
		return nil
	}
	active, err := valueForKey(&secret.Tags, "active")
	if err != nil {
		return nil
	}

	return &token{
		ID:     secret.Name,
		Name:   secret.Name,
		Value:  tok,
		Active: "1" == active,
	}
}

func isClientToken(secret *Secret) bool {
	namespace, err := valueForKey(&secret.Tags, "namespace")
	if err != nil {
		return false
	}
	return namespace == "clientTokens"
}

func valueForKey(tags *[]Tag, key string) (string, error) {
	for _, tag := range *tags {
		if tag.Key == key {
			return tag.Value, nil
		}
	}
	return "", fmt.Errorf("couldn't find a tag with key %v", key)
}
