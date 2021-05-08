package integration_test

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"time"
)

// notificationReceiver is an HTTP server that accepts notification posts from
// the notifications-service. We use it to verify that the
// notifications-service sends correctly formatted notifications in response to
// events. It's a small wrapper around the httptest library.
// The HTTP server listens on a random port each time you start it. Use the
// SlackURL(), etc. functions to get a useable URL.
type notificationReceiver struct {
	server *httptest.Server
	posts  chan *notificationPost
}

// notificationPost contains data from requests made by the notifications
// service to the notificationReceiver server. Add any fields you need to
// verify the notifications-service's behavior
type notificationPost struct {
	URL               string
	BodyBytes         []byte
	BasicAuthUsed     bool
	BasicAuthUsername string
	BasicAuthPassword string
}

func newTestServer() *notificationReceiver {
	n := notificationReceiver{
		posts: make(chan *notificationPost),
	}

	n.server = httptest.NewServer(n.httpHandler())
	return &n
}

func (n *notificationReceiver) Close() {
	n.server.Close()
	close(n.posts)
}

func (n *notificationReceiver) SlackURL() string {
	return fmt.Sprintf("%s/slackalert/", n.server.URL)
}

func (n *notificationReceiver) ServiceNowURL() string {
	return fmt.Sprintf("%s/servicenowalert/", n.server.URL)
}

func (n *notificationReceiver) WebhookURL() string {
	return fmt.Sprintf("%s/webhookalert/", n.server.URL)
}

func (n *notificationReceiver) GetLastPost() (*notificationPost, error) {
	timeoutSig := make(chan (int))
	timeoutLimit := 5 * time.Second
	canceler := time.AfterFunc(timeoutLimit, func() {
		timeoutSig <- 1
	})

	select {
	case p := <-n.posts:
		canceler.Stop()
		return p, nil
	case <-timeoutSig:
		return nil, fmt.Errorf("no post received by the test server before timeout of %s", timeoutLimit)
	}
}

// CheckForPost: Check if there is a post in the posts channel, and return it
// if it's there.  This is here to make negative tests (i.e., you expect no
// alert to be sent) more robust and easier to debug. You assert that this
// function returns nil and if it's not nil, testify will tell you a bit about
// the value you did get.
//
// It sleeps for 1 second because notifications-service sends notifications
// async and there's no good way to synchronize on the absence of a message.
func (n *notificationReceiver) CheckForPost() *notificationPost {
	var p *notificationPost
	time.Sleep(1 * time.Second)
	select {
	case p = <-n.posts:
		return p
	default:
		return p
	}
}

func (n *notificationReceiver) httpHandler() http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		//fmt.Printf("Test server recvd request: %s %s\n", r.Method, r.URL)

		if r.Method == "POST" {
			bodyBytes, err := ioutil.ReadAll(r.Body)
			if err != nil {
				fmt.Printf("[TEST SERVER] READ ERROR: %s\n", err)
				return
			}
			//fmt.Printf("Body: \n%s\n----\n", bodyBytes)

			// posts channel is synchronous (unbuffered). We desire to have synchronous
			// behavior in the test but not block this server forever if a test forgets
			// to read from the channel. So do the send in a goroutine
			go func() {
				p := &notificationPost{
					URL:       r.URL.String(),
					BodyBytes: bodyBytes,
				}
				p.BasicAuthUsername, p.BasicAuthPassword, p.BasicAuthUsed = r.BasicAuth()

				n.posts <- p
			}()
		}

		fmt.Fprintln(w, "PONG")
	}
}
