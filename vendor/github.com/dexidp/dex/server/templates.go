package server

import (
	"fmt"
	"html/template"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

const (
	tmplApproval = "approval.html"
	tmplLogin    = "login.html"
	tmplPassword = "password.html"
	tmplOOB      = "oob.html"
	tmplError    = "error.html"
)

var requiredTmpls = []string{
	tmplApproval,
	tmplLogin,
	tmplPassword,
	tmplOOB,
	tmplError,
}

type templates struct {
	loginTmpl    *template.Template
	approvalTmpl *template.Template
	passwordTmpl *template.Template
	oobTmpl      *template.Template
	errorTmpl    *template.Template
}

type webConfig struct {
	dir       string
	logoURL   string
	issuer    string
	theme     string
	issuerURL string
	extra     map[string]string
}

func join(base, path string) string {
	b := strings.HasSuffix(base, "/")
	p := strings.HasPrefix(path, "/")
	switch {
	case b && p:
		return base + path[1:]
	case b || p:
		return base + path
	default:
		return base + "/" + path
	}
}

func dirExists(dir string) error {
	stat, err := os.Stat(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf("directory %q does not exist", dir)
		}
		return fmt.Errorf("stat directory %q: %v", dir, err)
	}
	if !stat.IsDir() {
		return fmt.Errorf("path %q is a file not a directory", dir)
	}
	return nil
}

// loadWebConfig returns static assets, theme assets, and templates used by the frontend by
// reading the directory specified in the webConfig.
//
// The directory layout is expected to be:
//
//    ( web directory )
//    |- static
//    |- themes
//    |  |- (theme name)
//    |- templates
//
func loadWebConfig(c webConfig) (static, theme http.Handler, templates *templates, err error) {
	if c.theme == "" {
		c.theme = "coreos"
	}
	if c.issuer == "" {
		c.issuer = "dex"
	}
	if c.dir == "" {
		c.dir = "./web"
	}
	if c.logoURL == "" {
		c.logoURL = join(c.issuerURL, "theme/logo.png")
	}

	if err := dirExists(c.dir); err != nil {
		return nil, nil, nil, fmt.Errorf("load web dir: %v", err)
	}

	staticDir := filepath.Join(c.dir, "static")
	templatesDir := filepath.Join(c.dir, "templates")
	themeDir := filepath.Join(c.dir, "themes", c.theme)

	for _, dir := range []string{staticDir, templatesDir, themeDir} {
		if err := dirExists(dir); err != nil {
			return nil, nil, nil, fmt.Errorf("load dir: %v", err)
		}
	}

	static = http.FileServer(http.Dir(staticDir))
	theme = http.FileServer(http.Dir(themeDir))

	templates, err = loadTemplates(c, templatesDir)
	return
}

// loadTemplates parses the expected templates from the provided directory.
func loadTemplates(c webConfig, templatesDir string) (*templates, error) {
	files, err := ioutil.ReadDir(templatesDir)
	if err != nil {
		return nil, fmt.Errorf("read dir: %v", err)
	}

	filenames := []string{}
	for _, file := range files {
		if file.IsDir() {
			continue
		}
		filenames = append(filenames, filepath.Join(templatesDir, file.Name()))
	}
	if len(filenames) == 0 {
		return nil, fmt.Errorf("no files in template dir %q", templatesDir)
	}

	funcs := map[string]interface{}{
		"issuer": func() string { return c.issuer },
		"logo":   func() string { return c.logoURL },
		"url":    func(s string) string { return join(c.issuerURL, s) },
		"lower":  strings.ToLower,
		"extra":  func(k string) string { return c.extra[k] },
	}

	tmpls, err := template.New("").Funcs(funcs).ParseFiles(filenames...)
	if err != nil {
		return nil, fmt.Errorf("parse files: %v", err)
	}
	missingTmpls := []string{}
	for _, tmplName := range requiredTmpls {
		if tmpls.Lookup(tmplName) == nil {
			missingTmpls = append(missingTmpls, tmplName)
		}
	}
	if len(missingTmpls) > 0 {
		return nil, fmt.Errorf("missing template(s): %s", missingTmpls)
	}
	return &templates{
		loginTmpl:    tmpls.Lookup(tmplLogin),
		approvalTmpl: tmpls.Lookup(tmplApproval),
		passwordTmpl: tmpls.Lookup(tmplPassword),
		oobTmpl:      tmpls.Lookup(tmplOOB),
		errorTmpl:    tmpls.Lookup(tmplError),
	}, nil
}

var scopeDescriptions = map[string]string{
	"offline_access": "Have offline access",
	"profile":        "View basic profile information",
	"email":          "View your email address",
}

type connectorInfo struct {
	ID   string
	Name string
	URL  string
}

type byName []connectorInfo

func (n byName) Len() int           { return len(n) }
func (n byName) Less(i, j int) bool { return n[i].Name < n[j].Name }
func (n byName) Swap(i, j int)      { n[i], n[j] = n[j], n[i] }

func (t *templates) login(w http.ResponseWriter, connectors []connectorInfo) error {
	sort.Sort(byName(connectors))
	data := struct {
		Connectors []connectorInfo
	}{connectors}
	return renderTemplate(w, t.loginTmpl, data)
}

func (t *templates) password(w http.ResponseWriter, postURL, lastUsername, usernamePrompt string, lastWasInvalid, showBacklink bool) error {
	data := struct {
		PostURL        string
		BackLink       bool
		Username       string
		UsernamePrompt string
		Invalid        bool
	}{postURL, showBacklink, lastUsername, usernamePrompt, lastWasInvalid}
	return renderTemplate(w, t.passwordTmpl, data)
}

func (t *templates) approval(w http.ResponseWriter, authReqID, username, clientName string, scopes []string) error {
	accesses := []string{}
	for _, scope := range scopes {
		access, ok := scopeDescriptions[scope]
		if ok {
			accesses = append(accesses, access)
		}
	}
	sort.Strings(accesses)
	data := struct {
		User      string
		Client    string
		AuthReqID string
		Scopes    []string
	}{username, clientName, authReqID, accesses}
	return renderTemplate(w, t.approvalTmpl, data)
}

func (t *templates) oob(w http.ResponseWriter, code string) error {
	data := struct {
		Code string
	}{code}
	return renderTemplate(w, t.oobTmpl, data)
}

func (t *templates) err(w http.ResponseWriter, errCode int, errMsg string) error {
	w.WriteHeader(errCode)
	data := struct {
		ErrType string
		ErrMsg  string
	}{http.StatusText(errCode), errMsg}
	if err := t.errorTmpl.Execute(w, data); err != nil {
		return fmt.Errorf("Error rendering template %s: %s", t.errorTmpl.Name(), err)
	}
	return nil
}

// small io.Writer utility to determine if executing the template wrote to the underlying response writer.
type writeRecorder struct {
	wrote bool
	w     io.Writer
}

func (w *writeRecorder) Write(p []byte) (n int, err error) {
	w.wrote = true
	return w.w.Write(p)
}

func renderTemplate(w http.ResponseWriter, tmpl *template.Template, data interface{}) error {
	wr := &writeRecorder{w: w}
	if err := tmpl.Execute(wr, data); err != nil {
		if !wr.wrote {
			// TODO(ericchiang): replace with better internal server error.
			http.Error(w, "Internal server error", http.StatusInternalServerError)
		}
		return fmt.Errorf("Error rendering template %s: %s", tmpl.Name(), err)
	}
	return nil
}
