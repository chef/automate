package server

import (
	"os"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
)

func (s *server) ReconfigureHandler(ch chan os.Signal, grpcServer *grpc.Server) {
	for {
		<-ch
		logrus.Info("HUP received, reconfiguring")
		if s.deployment.Config == nil {
			logrus.Error("Ignoring reconfigure request because we don't have any configuration.")
			continue
		}

		sysConfig := s.deployment.Config.GetDeployment().GetV1().GetSys()
		restartRequired := s.applyServerConfiguration(sysConfig)
		if restartRequired {
			logrus.Info("Restart required to apply configuration. Restarting now.")
			grpcServer.GracefulStop()
			os.Exit(0)
		}
		err := s.target().UnsetDeploymentServiceReconfigurePending()
		if err != nil {
			logrus.Error("Failed to unset reconfigure-pending sentinel on deployment-service")
		}
	}
}

func (s *server) applyServerConfiguration(c *dc.ConfigRequest_V1_System) bool {
	s.setLogLevel(c.GetLog())
	s.setGatherLogsStagingDir(c.GetGatherLogs())

	restartRequired := s.checkProxyEnv(c.GetProxy())
	restartRequired = s.checkServiceConfig(c.GetService()) || restartRequired
	return restartRequired
}

func (s *server) setLogLevel(logConfig *dc.ConfigRequest_V1_System_Log) {
	newLogLevel := logConfig.GetLevel().GetValue()
	oldLogLevel := logrus.GetLevel().String()

	if newLogLevel != oldLogLevel {
		logrus.WithFields(logrus.Fields{
			"old_log_level": oldLogLevel,
			"new_log_level": newLogLevel,
		}).Info("Reconfiguring log level")
		setLogrusLevel(newLogLevel)
		s.serverConfig.LogLevel = newLogLevel
	}
}

func setLogrusLevel(newLogLevel string) {
	var err error
	level := logrus.InfoLevel
	if newLogLevel != "" {
		level, err = logrus.ParseLevel(newLogLevel)
		if err != nil {
			logrus.WithField("level", newLogLevel).WithError(err).Error("could not parse log level using default level of 'info'")
		}
	}
	logrus.SetLevel(level)
}

func (s *server) setGatherLogsStagingDir(glConfig *dc.ConfigRequest_V1_System_GatherLogs) {
	newStagingDir := glConfig.GetStagingDir().GetValue()
	oldStagingDir := s.serverConfig.StagingDir
	if newStagingDir != oldStagingDir {
		logrus.WithFields(logrus.Fields{
			"old_staging_dir": oldStagingDir,
			"new_staging_dir": newStagingDir,
		}).Info("Reconfiguring gather-logs staging directory")
		s.serverConfig.StagingDir = newStagingDir
	}
}

func (s *server) checkProxyEnv(proxyConfig *dc.ConfigRequest_V1_System_Proxy) bool {
	newProxy := proxyConfig.GetConnectionString().GetValue()
	newNoProxy := proxyConfig.GetNoProxyString().GetValue()
	oldProxy := os.Getenv("http_proxy")
	oldNoProxy := os.Getenv("no_proxy")
	restartRequired := false

	if newProxy != oldProxy {
		logrus.WithFields(logrus.Fields{
			"old_http_proxy": oldProxy,
			"new_http_proxy": newProxy,
		}).Info("HTTP proxy settings have changed, restart required")

		restartRequired = true
	}

	if newNoProxy != oldNoProxy {
		logrus.WithFields(logrus.Fields{
			"old_noproxy": oldNoProxy,
			"new_noproxy": newNoProxy,
		}).Info("HTTP noproxy settings have changed, restart required")

		restartRequired = true
	}

	return restartRequired
}

func (s *server) checkServiceConfig(serviceConfig *dc.ConfigRequest_V1_System_Service) bool {
	restartRequired := false

	oldListenAddress := s.serverConfig.ListenAddress
	oldPort := s.serverConfig.Port
	newListenAddress := serviceConfig.GetListenAddress().GetValue()
	newPort := serviceConfig.GetPort().GetValue()

	if uint32(newPort) != oldPort {
		logrus.WithFields(logrus.Fields{
			"old_port": oldPort,
			"new_port": newPort,
		}).Info("Listen port has changed. Restart is required")
		restartRequired = true
	}

	if newListenAddress != oldListenAddress {
		logrus.WithFields(logrus.Fields{
			"old_address": oldListenAddress,
			"new_address": newListenAddress,
		}).Info("Listen address has changed. Restart is required")
		restartRequired = true
	}

	return restartRequired
}
