package commands

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"time"

	"crypto/tls"
	"net/http"

	"golang.org/x/crypto/bcrypt"
	"gopkg.in/ini.v1"
	"gopkg.in/yaml.v2"
)

type ElasticSidecar struct {
	logger *log.Logger
	config *Config
}

type Config struct {
	AdminUsername      string `ini:"admin_username"`
	AdminPassword      string `ini:"admin_password"`
	JavaHome           string `ini:"java_home"`
	ToolPath           string `ini:"tool_path"`
	OpensearchIP       string `ini:"opensearch_ip"`
	OpensearchPort     string `ini:"opensearch_port"`
	OpensearchCA       string `ini:"opensearch_ca"`
	AdminCert          string `ini:"admin_cert"`
	AdminKey           string `ini:"admin_key"`
	SecurityConfigPath string `ini:"securityconfig_path"`
}

func NewElasticSidecar() *ElasticSidecar {
	logger := log.New(os.Stderr, "", log.LstdFlags)
	logger.Println("AutomateCluster::ElasticSidecar initialized!")
	return &ElasticSidecar{
		logger: logger,
		config: loadConfig(),
	}
}

func loadConfig() *Config {
	cfg := new(Config)
	iniFile, err := ini.Load("/hab/svc/automate-ha-elasticsidecar/config/elastic_sidecar.toml")
	if err != nil {
		log.Fatalf("Failed to read config file: %v", err)
	}
	err = iniFile.MapTo(cfg)
	if err != nil {
		log.Fatalf("Failed to map config: %v", err)
	}
	return cfg
}

func (es *ElasticSidecar) validJSON(jsonStr string) bool {
	var js json.RawMessage
	return json.Unmarshal([]byte(jsonStr), &js) == nil
}

func (es *ElasticSidecar) testAuthentication(user, pass string) (int, string) {
	client := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{InsecureSkipVerify: true}, // nosemgrep
		},
	}
	req, err := http.NewRequest("GET", "https://localhost:9200", nil)
	if err != nil {
		es.logger.Println(err)
		return 0, "error"
	}
	req.SetBasicAuth(user, pass)
	resp, err := client.Do(req)
	if err != nil {
		es.logger.Println(err)
		return 0, "error"
	}
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)
	return resp.StatusCode, string(body)
}

func (es *ElasticSidecar) runCommand(cmd string) error {
	cmdExec := exec.Command("bash", "-c", cmd)
	cmdExec.Env = append(os.Environ(), fmt.Sprintf("HAB_LICENSE=accept-no-persist"), fmt.Sprintf("JAVA_HOME=%s", es.config.JavaHome))
	es.logger.Println("Running CMD:", cmd)
	output, err := cmdExec.CombinedOutput()
	if err != nil {
		es.logger.Println("encountered exception:", err)
		es.logger.Println(string(output))
		return err
	}
	return nil
}

func (es *ElasticSidecar) hashPassword(password string) (string, error) {
	bytes, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	return string(bytes), err
}

func (es *ElasticSidecar) writeInternalUsers() {
	pwd, _ := es.hashPassword(es.config.AdminPassword)
	internalUsers := map[string]interface{}{
		"_meta": map[string]interface{}{
			"type":           "internalusers",
			"config_version": "2",
		},
		es.config.AdminUsername: map[string]interface{}{
			"hash":          pwd,
			"reserved":      true,
			"backend_roles": []string{es.config.AdminUsername},
		},
		"kibanaserver": map[string]interface{}{
			"hash":     pwd,
			"reserved": true,
		},
		"kibanaro": map[string]interface{}{
			"hash":          pwd,
			"reserved":      false,
			"backend_roles": []string{"kibanauser", "readall"},
		},
		"logstash": map[string]interface{}{
			"hash":          pwd,
			"reserved":      false,
			"backend_roles": []string{"logstash"},
		},
		"readall": map[string]interface{}{
			"hash":          pwd,
			"reserved":      false,
			"backend_roles": []string{"readall"},
		},
		"snapshotrestore": map[string]interface{}{
			"hash":          pwd,
			"reserved":      false,
			"backend_roles": []string{"snapshotrestore"},
		},
	}
	data, _ := yaml.Marshal(internalUsers)
	ioutil.WriteFile(fmt.Sprintf("%s/internal_users.yml", es.config.SecurityConfigPath), data, 0644)
}

func (es *ElasticSidecar) writeRolesMapping() {
	rolesMapping := map[string]interface{}{
		"_meta": map[string]interface{}{
			"type":           "rolesmapping",
			"config_version": "2",
		},
		"all_access": map[string]interface{}{
			"reserved":      false,
			"backend_roles": []string{es.config.AdminUsername},
		},
		"own_index": map[string]interface{}{
			"reserved":      false,
			"backend_roles": []string{"*"},
		},
		"logstash": map[string]interface{}{
			"reserved":      false,
			"backend_roles": []string{"logstash"},
		},
		"kibana_user": map[string]interface{}{
			"reserved":      false,
			"backend_roles": []string{"kibanauser"},
		},
		"readall": map[string]interface{}{
			"reserved":      false,
			"backend_roles": []string{"readall"},
		},
		"manage_snapshots": map[string]interface{}{
			"reserved":      false,
			"backend_roles": []string{"snapshotrestore"},
		},
		"kibana_server": map[string]interface{}{
			"reserved":      true,
			"backend_roles": []string{"kibanaserver"},
		},
	}
	data, _ := yaml.Marshal(rolesMapping)
	ioutil.WriteFile(fmt.Sprintf("%s/roles_mapping.yml", es.config.SecurityConfigPath), data, 0644)
}

func (es *ElasticSidecar) insertCredentials() {
	insertCommand := fmt.Sprintf("%s/securityadmin.sh -h %s -p %s -cacert %s -cert %s -key %s -nhnv -icl -cd %s",
		es.config.ToolPath, es.config.OpensearchIP, es.config.OpensearchPort, es.config.OpensearchCA,
		es.config.AdminCert, es.config.AdminKey, es.config.SecurityConfigPath)
	err := es.runCommand(insertCommand)
	if err != nil {
		es.logger.Println("Credentials failed to rotate with error:", err)
	} else {
		es.logger.Println("Credentials successfully rotated")
	}
}

func (es *ElasticSidecar) rotateCredentials() {
	es.logger.Println("Preparing to rotate credentials")
	es.writeInternalUsers()
	es.writeRolesMapping()
	es.insertCredentials()
}

func (es *ElasticSidecar) wait() {
	es.logger.Println("Sleeping 60s")
	time.Sleep(60 * time.Second)
}

func (es *ElasticSidecar) run() {
	for {
		code, response := es.testAuthentication(es.config.AdminUsername, es.config.AdminPassword)
		if response == "error" {
			es.wait()
			continue
		} else if code == 200 {
			es.logger.Println("auth successful for", es.config.AdminUsername)
		}

		switch code {
		case 200:
			es.logger.Println("Authentication successful, doing nothing")
		case 401, 403:
			es.logger.Println("Authentication failed, inserting credentials")
			es.rotateCredentials()
		case 503:
			if response == "OpenSearch security not initialized" {
				es.logger.Println("OpenSearch security appears to not be setup, inserting credentials")
				es.rotateCredentials()
			} else if es.validJSON(response) {
				es.logger.Println("Auth successful, but OpenSearch appears to be broken, doing nothing.")
			} else {
				es.logger.Println("OpenSearch returned 503 error with unexpected message")
				es.logger.Println(response)
			}
		default:
			es.logger.Println("OpenSearch returned", code, "code with unexpected message")
			es.logger.Println(response)
		}
		es.wait()
	}
}

func Execute() {
	svc := NewElasticSidecar()
	svc.run()
}
