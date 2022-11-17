package main

import (
	"fmt"

	"github.com/chef/toml"
)
func getOpensearchConfig(sshUtil SSHUtil, remoteIps []string) (string, error) {
	scriptCommands := `sudo source /hab/sup/default/SystemdEnvironmentFile.sh
	sudo automate-backend-ctl show --svc=automate-ha-opensearch`
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return "", err
	}
	var array []map[string] interface{}
	for i := 0; i < len(remoteIps); i++ {
		sshUtil.getSSHConfig().hostIP = remoteIps[i]
		rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			writer.Error(err.Error())
			return "",err
		}
	writer.Println(rawOutput)
	//converting to struct
	var src OpensearchConfig
	if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
		return "", err
	}
		maparray := filteringRequiredFieldsFromOpensearchConfig(infra, &src)
		writer.Println(maparray["ip"],maparray["public_key"])
		array= append(array, maparray)
		
		}
	return "", nil
}

func getPostgresConfig(fileName string, timestamp string, config string, sshUser string, sshPort string, sskKeyFile string, remoteIP string) (string, error) {
	scriptCommands := `sudo source /hab/sup/default/SystemdEnvironmentFile.sh
	sudo automate-backend-ctl show --svc=automate-ha-postgresql`
	rawOutput, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIP, scriptCommands)
	if err != nil {
		return "", err
	}
	writer.Println(rawOutput)
	//converting to struct
	var src PostgresqlConfig
	if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
		return "", err
	}
	return "", nil
}
func filteringRequiredFieldsFromOpensearchConfig(infra *AutomteHAInfraDetails, osconfig *OpensearchConfig) map[string]interface{} {
	userData := make(map[string]interface{})
	userData ["ip"] = infra.Outputs.OpensearchPrivateIps.Value
	userData ["public_key"] = osconfig.TLS.SslCert
	userData ["private_key"] = osconfig.TLS.SslKey

	//mapArraysMap1["opensearch"] = map[string]string{{"ip": infra.Outputs.OpensearchPrivateIps.Value},{"public_key": osconfig.TLS.SslCert}, {"private_key": osconfig.TLS.SslKey}}
	return userData
}

func filteringRequiredFieldsFromPostgresqlConfig(infra *AutomteHAInfraDetails, pgconfig *PostgresqlConfig) map[string][]map[string]string {
	mapArraysMap1["postgresql"] =[]map[string]string{ {"ip": infra.Outputs.PostgresqlPrivateIps.Value[i]},{"public_key": pgconfig.Ssl.SslCert}, {"private_key": pgconfig.Ssl.SslKey}}
	return mapArraysMap1
}