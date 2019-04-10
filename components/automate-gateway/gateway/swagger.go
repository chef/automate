package gateway

import (
	"net/http"

	"github.com/sirupsen/logrus"

	swagger "github.com/chef/automate/components/automate-gateway/api"
)

// expose files in third_party/swagger-ui/
func serveOpenAPIUI(dir string) *http.ServeMux {
	mux := http.NewServeMux()
	if dir != "" {
		logrus.WithField("dir", dir).Info("Registering openapi-ui")
		fileServer := http.FileServer(http.Dir(dir))
		mux.Handle("/", fileServer)
	}
	mux.HandleFunc("/config.json", swagger.Swagger.HandleSwaggerUIConfig)
	return mux
}
