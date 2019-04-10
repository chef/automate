package lbrequest

import (
	"bytes"
	"encoding/json"
	"io"
	"mime/multipart"
	"net/http"
	"net/url"
	"strings"
	"text/template"

	"github.com/sirupsen/logrus"
)

// Opts are functional options for the automate load balancer http request
type Opts func(*requestOpts)

type requestOpts struct {
	method           string
	authToken        string
	defaultAuthToken string
	target           url.URL
	bodyFunc         func() (io.Reader, error)
	formFile         struct {
		filename string
		data     []byte
	}
}

// WithMethod sets the http method to use. By default, it is GET
func WithMethod(method string) Opts {
	return func(opts *requestOpts) {
		opts.method = method
	}
}

// WithAuthToken sets the auth token to use
func WithAuthToken(token string) Opts {
	return func(opts *requestOpts) {
		opts.authToken = token
	}
}

// WithDefaultAuthToken sets the auth token to use in the case that
// WithAuthToken never set an auth token. We use it to set the admin
// token
func WithDefaultAuthToken(token string) Opts {
	return func(opts *requestOpts) {
		opts.defaultAuthToken = token
	}
}

// WithJSONObjectBody sets the body of the request to a JSON marshalled
// object
func WithJSONObjectBody(body interface{}) Opts {
	data, err := json.Marshal(body)
	if err != nil {
		panic(err)
	}
	return func(opts *requestOpts) {
		opts.bodyFunc = func() (io.Reader, error) {
			return bytes.NewReader(data), nil
		}
	}
}

// WithJSONBody sets the body of a request. body should be a string
// containing json
func WithJSONBody(body string) Opts {
	return func(opts *requestOpts) {
		opts.bodyFunc = func() (io.Reader, error) {
			return strings.NewReader(body), nil
		}
	}
}

// WithJSONStringTemplateBody renders the given json template with the given parameters
// as the request body
func WithJSONStringTemplateBody(tmpl string, params interface{}) Opts {
	return func(opts *requestOpts) {
		opts.bodyFunc = func() (io.Reader, error) {
			tmpl := template.Must(template.New("").Parse(tmpl))
			buf := bytes.NewBuffer([]byte{})
			err := tmpl.Execute(buf, params)
			if err != nil {
				return nil, err
			}
			return buf, nil
		}
	}
}

// WithFormFile does a multipart form request
func WithFormFile(filename string, data []byte) Opts {
	return func(opts *requestOpts) {
		opts.formFile.filename = filename
		opts.formFile.data = data
	}
}

// WithHost sets the host the request should be made to.
func WithURL(url url.URL) Opts {
	return func(opts *requestOpts) {
		opts.target = url
	}
}

func mustParseURL(u string) url.URL {
	parsed, err := url.Parse(u)
	if err != nil {
		panic(err)
	}
	return *parsed
}

// New creates an automate load balancer HTTP request with the
// modifications given in opts.
func New(urlPath string, opts ...Opts) (*http.Request, error) {
	optsStruct := requestOpts{
		method: "GET",
		target: mustParseURL("https://localhost"),
		bodyFunc: func() (io.Reader, error) {
			return nil, nil
		},
	}

	for _, o := range opts {
		o(&optsStruct)
	}

	parsedURL, err := optsStruct.target.Parse(urlPath)
	if err != nil {
		panic(err)
	}
	var req *http.Request
	if optsStruct.formFile.data != nil {
		var b bytes.Buffer
		w := multipart.NewWriter(&b)
		var fw io.Writer
		if fw, err = w.CreateFormFile("file", optsStruct.formFile.filename); err != nil {
			return nil, err
		}
		if _, err = io.Copy(fw, bytes.NewReader(optsStruct.formFile.data)); err != nil {
			return nil, err
		}
		_ = w.Close()
		req, err = http.NewRequest(optsStruct.method, parsedURL.String(), &b)
		if err != nil {
			return nil, err
		}
		req.Header.Set("Content-Type", w.FormDataContentType())
	} else {
		body, err := optsStruct.bodyFunc()
		if err != nil {
			return nil, err
		}

		req, err = http.NewRequest(optsStruct.method, parsedURL.String(), body)
		if err != nil {
			return nil, err
		}
	}

	if optsStruct.authToken != "" {
		req.Header.Add("api-token", optsStruct.authToken)
	} else if optsStruct.defaultAuthToken != "" {
		req.Header.Add("api-token", optsStruct.defaultAuthToken)
	}

	logrus.Debug(req)

	return req, err
}
