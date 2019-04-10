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
	tokensURL  = "https://localhost/api/v0/auth/tokens"    // nolint
	secretsURL = "https://localhost/api/v0/secrets/search" // nolint
)

type Secrets struct {
	Secrets []Secret
}

type Secret struct {
	Id   string
	Name string
	Tags []Tag
}

type Tag struct {
	Key   string
	Value string
}

type token struct {
	Description string `json:"description"`
	Token       string `json:"token"`
	Active      bool   `json:"active"`
}

func main() {
	http.DefaultTransport.(*http.Transport).TLSClientConfig = &tls.Config{InsecureSkipVerify: true} // nolint

	flag.Parse()

	fmt.Printf("migrating tokens from secrets service to authz service using:\n")
	fmt.Printf("\tsecrets URL: %s\n", secretsURL)
	fmt.Printf("\ttokens URL: %s\n", tokensURL)
	fmt.Printf("\tapi token: %s\n\n", *authToken)

	tokens := getTokensFromSecrets()
	fmt.Printf("tokens  read from secrets service...\n")
	for _, t := range tokens {
		fmt.Printf("\treading token '%s' from secrets\n", t.Description)
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
	req.Header.Set("x-data-collector-token", *authToken)

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
	fmt.Printf("writing tokens to authz service...\n")
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
		req.Header.Set("x-data-collector-token", *authToken)

		_, err = client.Do(req)
		if err != nil {
			fmt.Printf("\tFAILED to write token %s, error: %v\n", t.Description, err)
		} else {
			fmt.Printf("\twrote token %q successfully\n", t.Description)
			successfulWrites += 1
		}
	}
	fmt.Printf("\ttotal tokens written to authz service: %d\n", successfulWrites)
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
		Description: secret.Name,
		Token:       tok,
		Active:      "1" == active,
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
