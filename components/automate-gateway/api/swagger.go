package api

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"path/filepath"
	"sort"
	"strings"
)

type swagger struct {
	definitions map[string]string
}

var (
	Swagger = new(swagger)
)

func (m *swagger) init() {
	if m.definitions == nil {
		m.definitions = make(map[string]string)
	}
}

func (m *swagger) Add(scope string, definition string) {
	m.init()
	m.definitions[scope] = definition
}

func (m *swagger) Get(scope string) string {
	m.init()
	return m.definitions[scope]
}

func (m *swagger) HandleSwaggerSpec(w http.ResponseWriter, r *http.Request) {
	name := nameFromPath(r.URL.Path)
	if docs := m.Get(name); docs != "" {
		io.Copy(w, strings.NewReader(docs)) // nolint: errcheck
		return
	}
	w.WriteHeader(http.StatusNotFound)
}

type swaggerUIConfig struct {
	Urls []swaggerUIConfigURL `json:"urls"`
}

type swaggerUIConfigURL struct {
	Name string `json:"name"`
	URL  string `json:"url"`
}

type swaggerUIConfigURLs []swaggerUIConfigURL

func (s swaggerUIConfigURLs) Len() int           { return len(s) }
func (s swaggerUIConfigURLs) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s swaggerUIConfigURLs) Less(i, j int) bool { return s[i].Name < s[j].Name }

func (m *swagger) HandleSwaggerUIConfig(w http.ResponseWriter, r *http.Request) {
	config := swaggerUIConfig{
		Urls: []swaggerUIConfigURL{},
	}
	for name := range m.definitions {
		config.Urls = append(config.Urls, swaggerUIConfigURL{
			Name: name,
			URL:  fmt.Sprintf("../%s.json", name),
		})
	}
	sort.Sort(swaggerUIConfigURLs(config.Urls))
	js, _ := json.Marshal(&config)
	w.Header().Set("Content-Type", "application/json")
	io.Copy(w, bytes.NewReader(js)) // nolint: errcheck
}

// "/notifications.json" => "notifications"
func nameFromPath(path string) string {
	basename := filepath.Base(path)
	return strings.TrimSuffix(basename, filepath.Ext(basename))
}
