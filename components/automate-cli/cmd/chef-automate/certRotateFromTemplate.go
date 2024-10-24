package main

import (
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
)

var automateStartedChan = make(chan bool)

func (c *certRotateFlow) certRotateFromTemplate(clusterCertificateFile string, sshUtil SSHUtil, infra *AutomateHAInfraDetails, currentCertsInfo *certShowCertificates, statusSummary StatusSummary, userConsent bool, waitTime time.Duration, flagsObj *certRotateFlags) error {
	totalWaitTimeOut := time.Duration(1000)
	if flagsObj.timeout > 0 {
		totalWaitTimeOut = time.Duration(flagsObj.timeout)
	}
	sshConfig := c.getSshDetails(infra)
	configRes := sshutils.SSHConfig{
		SshUser:    sshConfig.sshUser,
		SshPort:    sshConfig.sshPort,
		SshKeyFile: sshConfig.sshKeyFile,
		HostIP:     sshConfig.hostIP,
		Timeout:    sshConfig.timeout,
	}
	c.writer.Println("fetching certificates...")
	templateCerts, err := getCertsFromTemplate(clusterCertificateFile)
	if err != nil {
		return err
	}
	errs := c.validateCertificateTemplate(templateCerts, infra)
	if len(errs) > 0 {
		var errorMsg strings.Builder
		for _, er := range errs {
			errorMsg.WriteString(er.Error())
			errorMsg.WriteString("\n")
		}
		return errors.New(errorMsg.String())
	}
	c.log.Debug("==========================================================")
	c.log.Debug("Stopping traffic MAINTENANICE MODE ON")
	c.log.Debug("==========================================================")
	err = checkLagAndStopTraffic(infra, configRes, c.sshUtil, c.log, statusSummary, userConsent, waitTime, totalWaitTimeOut, c.writer)
	if err != nil {
		return err
	}

	defer func() {
		c.log.Debug("==========================================================")
		c.log.Debug("Defer Starting traffic MAINTENANICE MODE OFF")
		c.log.Debug("==========================================================")
		startTrafficOnAutomateNode(infra, configRes, c.sshUtil, c.log, c.writer, totalWaitTimeOut)
		startTrafficOnChefServerNode(infra, configRes, c.sshUtil, c.log, c.writer, totalWaitTimeOut)
	}()

	if templateCerts != nil {
		err = c.handleTemplateCertificateRotation(templateCerts, configRes, sshUtil, infra, currentCertsInfo, statusSummary, userConsent, waitTime, totalWaitTimeOut)
		if err != nil {
			return err
		}
	}
	return nil
}
func (c *certRotateFlow) handleTemplateCertificateRotation(templateCerts *CertificateToml, configRes sshutils.SSHConfig, sshUtil SSHUtil, infra *AutomateHAInfraDetails, currentCertsInfo *certShowCertificates, statusSummary StatusSummary, userConsent bool, waitTime time.Duration, totalWaitTimeOut time.Duration) error {

	// rotating PG certs
	start := time.Now()
	c.log.Debug("Started executing at %s \n", start.String())
	c.writer.Println("Rotating PostgreSQL certificates")
	pgRootCA := templateCerts.PostgreSQL.RootCA
	c.writer.Printf("Fetching PostgreSQL RootCA from template %s \n", pgRootCA)
	c.writer.Println("Rotating PostgreSQL follower node certificates")
	err := c.rotatePGCertAndRestartPGNode(templateCerts.PostgreSQL.IPS, statusSummary, infra, sshUtil, currentCertsInfo, pgRootCA, true)
	if err != nil {
		return err
	}
	c.writer.Println("PG certificate rotated and node restarted")
	timeElapsed := time.Since(start)
	c.log.Debug("Time elapsed to execute Postgresql certificate rotation since start %f \n", timeElapsed.Seconds())
	// rotating OS certs
	c.writer.Println("rotating opensearch node certificates")
	for i, osIp := range templateCerts.OpenSearch.IPS {
		c.writer.Printf("Rotating OpenSearch node %d certificates \n", i)
		err = c.rotateOSNodeCerts(infra, sshUtil, currentCertsInfo, &templateCerts.OpenSearch, &osIp, false)
		if err != nil {
			return err
		}
	}
	timeElapsed = time.Since(start)
	c.log.Debug("Time elapsed to execute Opensearch certificate rotation since start %f \n", timeElapsed.Seconds())
	automateIps := map[string]bool{}
	filterIps := []IP{}
	// rotate AutomateCerts
	for i, a2Ip := range templateCerts.Automate.IPS {
		c.writer.Printf("Rotating Automate node %d certificates \n", i)
		err := c.rotateAutomateNodeCerts(infra, sshUtil, currentCertsInfo, templateCerts, &a2Ip)
		if err != nil {
			return err
		}
		filterIps = append(filterIps, a2Ip)
		automateIps[a2Ip.IP] = true
	}
	automateStartedChan <- true

	timeElapsed = time.Since(start)
	c.log.Debug("Time elapsed to execute Automate certificate rotation since start %f \n", timeElapsed.Seconds())

	for i, csIp := range templateCerts.ChefServer.IPS {
		if _, ok := automateIps[csIp.IP]; !ok {
			c.writer.Printf("Rotating Chef Server node %d certificates \n", i)
			err := c.rotateChefServerNodeCerts(infra, sshUtil, currentCertsInfo, templateCerts, &csIp)
			if err != nil {
				return err
			}
			filterIps = append(filterIps, csIp)
		}

	}

	timeElapsed = time.Since(start)
	c.log.Debug("Time elapsed to execute ChefServer certificate rotation since start %f \n", timeElapsed.Seconds())

	//patch pg and opensearch root ca in frontend nodes
	if len(templateCerts.OpenSearch.RootCA) != 0 || len(templateCerts.PostgreSQL.RootCA) != 0 {
		_, err = c.patchPGOSRootCAOnFrontend(infra, sshUtil, currentCertsInfo, templateCerts, filterIps)
		if err != nil {
			return err
		}
	}

	c.log.Debug("==========================================================")
	c.log.Debug("Starting traffic on frontend nodes MAINTENANICE MODE OFF")
	c.log.Debug("==========================================================")

	startTrafficOnChefServerNode(infra, configRes, c.sshUtil, c.log, writer, totalWaitTimeOut)
	return nil
}

func (c *certRotateFlow) rotatePGCertAndRestartPGNode(pgIps []IP, statusSummary StatusSummary, infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, concurrent bool) error {
	pgLeaderIpAndHealth := getPGLeader(statusSummary)
	c.writer.Println("rotating follower node certificate")
	err := c.rotatePGFollowerNodeCert(pgIps, pgLeaderIpAndHealth, infra, sshUtil, currentCertsInfo, pgRootCA, concurrent)
	if err != nil {
		return err
	}
	c.writer.Println("rotating leader node certificate")
	err = c.rotatePGLeaderNodeCert(pgIps, pgLeaderIpAndHealth, infra, sshUtil, currentCertsInfo, pgRootCA, false)
	if err != nil {
		return err
	}
	c.writer.Println("restarting postgres service and reloading ha proxy")

	return c.nodeUtils.postPGCertRotate(infra.Outputs.PostgresqlPrivateIps.Value, *sshUtil.getSSHConfig(), c.fileUtils, c.log)
}

func (c *certRotateFlow) rotatePGLeaderNodeCert(pgIps []IP, pgLeaderIpAndHealth *NodeIpHealth, infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, concurrent bool) error {
	if pgLeaderIpAndHealth.Health == "OK" {
		for _, pgIp := range pgIps {
			if strings.EqualFold(pgIp.IP, pgLeaderIpAndHealth.IP) {
				err := c.rotatePGNodeCerts(infra, sshUtil, currentCertsInfo, pgRootCA, &pgIp, concurrent)
				if err != nil {
					return err
				}
				break
			}
		}
	}
	return nil
}

func (c *certRotateFlow) rotatePGFollowerNodeCert(pgIps []IP, pgLeaderIpAndHealth *NodeIpHealth, infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, concurrent bool) error {
	for _, pgIp := range pgIps {
		if !strings.EqualFold(pgIp.IP, pgLeaderIpAndHealth.IP) {
			err := c.rotatePGNodeCerts(infra, sshUtil, currentCertsInfo, pgRootCA, &pgIp, concurrent)
			//err := <-chErr
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *certRotateFlow) rotatePGNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, pgIps *IP, concurrent bool) error {
	start := time.Now()
	c.log.Debug("Roating PostgreSQL node %s certificate at %s \n", pgIps.IP, start.Format(time.ANSIC))
	if len(pgIps.PrivateKey) == 0 || len(pgIps.Publickey) == 0 {
		c.writer.Printf("Empty certificate for PostgerSQL node %s \n", pgIps.IP)
		err := errors.New(fmt.Sprintf("Empty certificate for PostgerSQL node %s \n", pgIps.IP))
		return err
	}
	flagsObj := certRotateFlags{
		postgres:        true,
		rootCAPath:      pgRootCA,
		privateCertPath: pgIps.PrivateKey,
		publicCertPath:  pgIps.Publickey,
		node:            pgIps.IP,
		timeout:         1000,
	}
	certs, err := c.getCerts(infra, &flagsObj)
	if err != nil {
		return err
	}

	if isManagedServicesOn() {
		err := status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, POSTGRESQL)
		return err
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := POSTGRESQL

	// Creating and patching the required configurations.
	config := fmt.Sprintf(POSTGRES_CONFIG, certs.privateCert, certs.publicCert, certs.rootCA)

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, &flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      &flagsObj,
		skipIpsList:   skipIpsList,
	}

	// patching on PG
	err = c.patchConfig(patchFnParam, false)
	if err != nil {
		return err
	}
	timeElapsed := time.Since(start)
	c.log.Debug("Time taken to rotate PostgreSQL node %s certificate at %f \n", pgIps.IP, timeElapsed.Seconds())
	return nil
}

func (c *certRotateFlow) rotateOSNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, oss *NodeCertficate, osIp *IP, concurrent bool) error {
	start := time.Now()
	c.log.Debug("Roating opensearch node %s certificate at %s \n", osIp.IP, start.Format(time.ANSIC))
	if len(osIp.PrivateKey) == 0 || len(osIp.Publickey) == 0 {
		c.writer.Printf("Empty certificate for OpenSearch node %s \n", osIp.IP)
		err := errors.New(fmt.Sprintf("Empty certificate for OpenSearch node %s \n", osIp.IP))
		return err
	}
	writer.Printf("Admin cert path : %s \n", oss.AdminPublickey)
	flagsObj := certRotateFlags{
		opensearch:      true,
		rootCAPath:      oss.RootCA,
		adminKeyPath:    oss.AdminPrivateKey,
		adminCertPath:   oss.AdminPublickey,
		privateCertPath: osIp.PrivateKey,
		publicCertPath:  osIp.Publickey,
		node:            osIp.IP,
		timeout:         1000,
	}
	certs, err := c.getCerts(infra, &flagsObj)
	if err != nil {
		return err
	}

	if isManagedServicesOn() {
		err := status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, OPENSEARCH)
		return err
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := OPENSEARCH
	adminPublicCert, err := c.getCertFromFile(oss.AdminPublickey, infra)
	if err != nil {
		return err
	}
	adminPublicCertString := strings.TrimSpace(string(adminPublicCert))
	adminPrivateCert, err := c.getCertFromFile(oss.AdminPrivateKey, infra)
	if err != nil {
		return err
	}
	adminDn, err := getDistinguishedNameFromKey(adminPublicCertString)
	if err != nil {
		c.writer.Printf("Error in decoding admin cert, not able to get adminDn \n")
		return err
	}
	nodeDn, err := getDistinguishedNameFromKey(certs.publicCert)
	if err != nil {
		c.writer.Printf("Error in decoding node cert, not able to get nodeDn \n")
		return err
	}
	existingNodesDN := strings.TrimSpace(currentCertsInfo.OpensearchCertsByIP[0].NodesDn)
	if strings.HasSuffix(existingNodesDN, `\n`) {
		i := strings.LastIndex(existingNodesDN, `\n`)
		existingNodesDN = existingNodesDN[:i] + strings.Replace(existingNodesDN[i:], `\n`, "", 1)
	}
	nodesDn := ""
	if strings.EqualFold(existingNodesDN, fmt.Sprintf("%v", nodeDn)) {
		nodesDn = fmt.Sprintf("%v", nodeDn)
	} else {
		nodesDn = fmt.Sprintf("%v\n", existingNodesDN) + "  - " + fmt.Sprintf("%v\n", nodeDn)
	}

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, &flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.

	config := fmt.Sprintf(OPENSEARCH_CONFIG, certs.rootCA, adminPublicCertString, strings.TrimSpace(string(adminPrivateCert)), certs.publicCert, certs.privateCert, fmt.Sprintf("%v", adminDn), fmt.Sprintf("%v", nodesDn))

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      &flagsObj,
		skipIpsList:   skipIpsList,
	}

	err = c.patchConfig(patchFnParam, true)
	if err != nil {
		return err
	}

	if flagsObj.node != "" && stringutils.SliceContains(skipIpsList, flagsObj.node) {
		return nil
	}

	if flagsObj.node != "" {

		err := patchOSNodeDN(&flagsObj, patchFnParam, c, nodesDn)
		if err != nil {
			return err
		}

	}
	timeElapsed := time.Since(start)
	c.log.Debug("Time taken to roate opensearch node %s certificate at %f \n", osIp.IP, timeElapsed.Seconds())
	return nil
}

func (c *certRotateFlow) rotateAutomateNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, a2Ip *IP) error {
	if len(a2Ip.PrivateKey) == 0 || len(a2Ip.Publickey) == 0 {
		c.writer.Printf("Empty certificate for Automte node %s \n", a2Ip.IP)
		return errors.New(fmt.Sprintf("Empty certificate for Automte node %s \n", a2Ip.IP))
	}
	flagsObj := certRotateFlags{
		automate:        true,
		rootCAPath:      certToml.Automate.RootCA,
		privateCertPath: a2Ip.PrivateKey,
		publicCertPath:  a2Ip.Publickey,
		node:            a2Ip.IP,
		timeout:         1000,
	}
	return c.rotateClusterFrontendCertificates(infra, sshUtil, flagsObj, currentCertsInfo, certToml)
}

func (c *certRotateFlow) rotateChefServerNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, csIp *IP) error {
	if len(csIp.PrivateKey) == 0 || len(csIp.Publickey) == 0 {
		c.writer.Printf("Empty certificate for Chef Server node %s \n", csIp.IP)
		return errors.New(fmt.Sprintf("Empty certificate for Chef Server node %s \n", csIp.IP))
	}
	flagsObj := certRotateFlags{
		chefserver:      true,
		rootCAPath:      certToml.ChefServer.RootCA,
		privateCertPath: csIp.PrivateKey,
		publicCertPath:  csIp.Publickey,
		node:            csIp.IP,
		timeout:         1000,
	}
	return c.rotateClusterFrontendCertificates(infra, sshUtil, flagsObj, currentCertsInfo, certToml)
}

func (c *certRotateFlow) patchPGOSRootCAOnFrontend(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, filterIps []IP) ([]string, error) {
	patchConfig := ""
	if len(certToml.OpenSearch.RootCA) != 0 {
		opensearchFlagsObj := certRotateFlags{
			opensearch:      true,
			rootCAPath:      certToml.OpenSearch.RootCA,
			adminKeyPath:    certToml.OpenSearch.AdminPrivateKey,
			adminCertPath:   certToml.OpenSearch.AdminPublickey,
			privateCertPath: certToml.OpenSearch.IPS[0].PrivateKey,
			publicCertPath:  certToml.OpenSearch.IPS[0].Publickey,
			node:            certToml.OpenSearch.IPS[0].IP,
			timeout:         1000,
		}
		opensearchCerts, err := c.getCerts(infra, &opensearchFlagsObj)
		nodeDn := pkix.Name{}
		nodeDn, err = getDistinguishedNameFromKey(opensearchCerts.publicCert)
		if err != nil {
			return nil, err
		}
		opensearchRootCA, err := c.getCertFromFile(certToml.OpenSearch.RootCA, infra)
		if err != nil {
			return nil, err
		}
		patchConfig = patchConfig + "\n" + fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, string(opensearchRootCA), nodeDn.CommonName)
	}
	if len(certToml.PostgreSQL.RootCA) != 0 {
		postgreSQLRootCA, err := c.getCertFromFile(certToml.PostgreSQL.RootCA, infra)
		if err != nil {
			return nil, err
		}
		patchConfig = patchConfig + "\n" + fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, string(postgreSQLRootCA))
	}

	frontendIps := infra.Outputs.AutomatePrivateIps.Value
	frontendIps = append(frontendIps, infra.Outputs.ChefServerPrivateIps.Value...)
	ipsTobeSkiped := []string{}
	for _, filterIp := range filterIps {
		ipsTobeSkiped = append(ipsTobeSkiped, filterIp.IP)
	}
	filteredIps := c.getFilteredIps(frontendIps, ipsTobeSkiped)

	fileName := "cert-rotate-pg-os-fe.toml"
	for _, fIp := range filteredIps {
		flagsObj := certRotateFlags{
			node:    fIp,
			timeout: 1000,
		}
		timestamp := time.Now().Format("20060102150405")
		patchFnParam := &patchFnParameters{
			sshUtil:       sshUtil,
			config:        patchConfig,
			fileName:      fileName,
			timestamp:     timestamp,
			remoteService: FRONTEND,
			concurrent:    true,
			infra:         infra,
			flagsObj:      &flagsObj,
			skipIpsList:   []string{},
		}
		err := c.patchConfig(patchFnParam, true)
		if err != nil {
			return nil, err
		}
	}
	return filteredIps, nil
}

func (c *certRotateFlow) rotateClusterFrontendCertificates(infra *AutomateHAInfraDetails, sshUtil SSHUtil, flagsObj certRotateFlags, currentCertsInfo *certShowCertificates, certToml *CertificateToml) error {
	certs, err := c.getCerts(infra, &flagsObj)
	if err != nil {
		return err
	}

	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if flagsObj.automate {
		remoteService = AUTOMATE
	} else if flagsObj.chefserver {
		remoteService = CHEF_SERVER
	}
	//get ips to exclude
	skipIpsList := []string{}

	nodeDn := pkix.Name{}
	patchConfig := ""
	if len(certToml.OpenSearch.IPS) > 0 {
		opensearchFlagsObj := certRotateFlags{
			opensearch:      true,
			rootCAPath:      certToml.OpenSearch.RootCA,
			adminKeyPath:    certToml.OpenSearch.AdminPrivateKey,
			adminCertPath:   certToml.OpenSearch.AdminPublickey,
			privateCertPath: certToml.OpenSearch.IPS[0].PrivateKey,
			publicCertPath:  certToml.OpenSearch.IPS[0].Publickey,
			node:            certToml.OpenSearch.IPS[0].IP,
			timeout:         1000,
		}
		opensearchCerts, err := c.getCerts(infra, &opensearchFlagsObj)
		nodeDn, err = getDistinguishedNameFromKey(opensearchCerts.publicCert)
		if err != nil {
			return err
		}
		opensearchRootCA, err := c.getCertFromFile(certToml.OpenSearch.RootCA, infra)
		if err != nil {
			return err
		}
		patchConfig = patchConfig + "\n" + fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, string(opensearchRootCA), nodeDn.CommonName)
	}
	if len(certToml.PostgreSQL.RootCA) > 0 {
		postgreSQLRootCA, err := c.getCertFromFile(certToml.PostgreSQL.RootCA, infra)
		if err != nil {
			return err
		}
		patchConfig = patchConfig + "\n" + fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, string(postgreSQLRootCA))
	}

	// Creating and patching the required configurations.
	patchConfig = patchConfig + "\n" + fmt.Sprintf(FRONTEND_CONFIG, certs.publicCert, certs.privateCert, certs.publicCert, certs.privateCert)

	concurrent := true
	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        patchConfig,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      &flagsObj,
		skipIpsList:   skipIpsList,
	}
	err = c.patchConfig(patchFnParam, true)
	if err != nil {
		return err
	}
	return nil
}

func (c *certRotateFlow) validateCertificateTemplate(template *CertificateToml, infra *AutomateHAInfraDetails) []error {
	errs := []error{}
	if len(template.PostgreSQL.RootCA) != 0 {
		RootCA, err := c.getCertFromFile(template.PostgreSQL.RootCA, infra)
		if err != nil {
			errs = append(errs, errors.Wrap(err, "PostgreSQL RootCA file not exist."))
		}
		errsNodes := c.validateNodeCerts(template.PostgreSQL.IPS, infra, RootCA)
		errs = append(errs, errsNodes...)
	}
	if len(template.OpenSearch.RootCA) != 0 {
		rootCA, err := c.getCertFromFile(template.OpenSearch.RootCA, infra)
		if err != nil {
			errs = append(errs, errors.Wrap(err, "OpenSearch RootCA file not exist."))
		}
		if len(template.OpenSearch.AdminPrivateKey) != 0 {
			adminPrivateKey, err := c.getCertFromFile(template.OpenSearch.AdminPrivateKey, infra)
			if err != nil {
				errs = append(errs, errors.Wrap(err, "OpenSearch Admin Private key file not exist."))
			}
			err = c.validatePrivateKey(adminPrivateKey)
			if err != nil {
				errs = append(errs, errors.Wrap(err, "Not able to verify OpenSearch Private key"))
			}
		}
		if len(template.OpenSearch.AdminPublickey) != 0 {
			_, err := c.getCertFromFile(template.OpenSearch.AdminPublickey, infra)
			if err != nil {
				errs = append(errs, errors.Wrap(err, "OpenSearch Admin Public key file not exist."))
			}
		}
		errsNodes := c.validateNodeCerts(template.OpenSearch.IPS, infra, rootCA)

		errs = append(errs, errsNodes...)
	}
	return errs
}

func (c *certRotateFlow) validateNodeCerts(ips []IP, infra *AutomateHAInfraDetails, rootCA []byte) []error {
	errs := []error{}
	for _, ip := range ips {
		if len(ip.PrivateKey) != 0 {
			private, err := c.getCertFromFile(ip.PrivateKey, infra)
			if err != nil {
				errs = append(errs, errors.Wrapf(err, "Node %s Private key file not exist.", ip.IP))
			}
			err = c.validatePrivateKey(private)
			if err != nil {
				errs = append(errs, errors.Wrapf(err, "Not able to verify Node %s Private key", ip.IP))
			}
		}

		if len(ip.Publickey) != 0 {
			public, err := c.getCertFromFile(ip.Publickey, infra)
			if err != nil {
				errs = append(errs, errors.Wrapf(err, "Node %s Public key file not exist.", ip.IP))
			}
			err = c.validatePublicCertsWithRootCA(rootCA, public)
			if err != nil {
				errs = append(errs, errors.Wrapf(err, "Not able to verify Node %s Public key with Root Certificate", ip.IP))
			}
		}
	}
	return errs
}

func (c *certRotateFlow) validatePrivateKey(cert []byte) error {
	block, _ := pem.Decode(cert)
	if block == nil {
		return errors.New("Failed to parse the certificate PEM")
	}
	if block.Type != "PRIVATE KEY" && block.Type != "CERTIFICATE" {
		return errors.New(fmt.Sprintf("Failed to parse the certificate PEM, unexpected type: %s", block.Type))
	}
	return nil
}

func (c *certRotateFlow) validatePublicCertsWithRootCA(rootCert []byte, publicCert []byte) error {
	roots := x509.NewCertPool()
	ok := roots.AppendCertsFromPEM(rootCert)
	if !ok {
		return errors.New("Fialed to pasrse root certificate")
	}
	publicBlock, _ := pem.Decode(publicCert)
	if publicBlock == nil {
		return errors.New("Failed to parse public certificate PEM")
	}

	publicCertificate, err := x509.ParseCertificate(publicBlock.Bytes)
	if err != nil {
		return errors.Wrap(err, "Failed to parse cerificate")
	}
	opts := x509.VerifyOptions{
		Roots: roots,
	}

	if _, err := publicCertificate.Verify(opts); err != nil {
		return errors.Wrap(err, "Failed to verify public certificate with root")
	}
	return nil
}
