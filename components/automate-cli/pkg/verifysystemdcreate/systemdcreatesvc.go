package verifysystemdcreate

import (
	"fmt"
	"os"
	"path/filepath"
	"text/template"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
)

const (
	INFO_LEVEL          = "info"
	DEBUG_LEVEL         = "debug"
	SERVICE_NAME        = "automate-verify"
	SERVICE_DESCRIPTION = "Service for automating verification"
	SERVICE_COMMAND     = "chef-automate verify serve"
	SYSTEMD_FILE        = "%s.service"
	TEMPLATE_NAME       = "systemd-service"
	TEMPLATE_TEXT       = `[Unit]
Description={{.ServiceDescription}}
After=network.target

[Service]
ExecStart={{.ServiceCommand}}
Restart=always
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
`
)

type systemdInput struct {
	ServiceName        string
	ServiceDescription string
	ServiceCommand     string
}

type CreateSystemdService struct {
	SystemdCreateUtils      SystemdCreateUtils
	BinaryDestinationFolder string
	SystemdLocation         string
	Debug                   bool
	Logger                  logger.Logger
	Writer                  *cli.Writer
}

func NewCreateSystemdService(
	systemdCreateUtils SystemdCreateUtils,
	binaryDestinationFolder string,
	systemdLocation string,
	debug bool,
	writer *cli.Writer) (*CreateSystemdService, error) {
	level := INFO_LEVEL
	if debug {
		level = DEBUG_LEVEL
	}
	l, err := logger.NewLoggerWithOut("text", level, writer)
	if err != nil {
		return nil, err
	}
	if systemdCreateUtils == nil {
		return nil, errors.New("SystemdCreateUtils cannot be nil")
	}
	if writer == nil {
		return nil, errors.New("Writer cannot be nil")
	}
	if binaryDestinationFolder == "" {
		return nil, errors.New("Binary destination folder cannot be empty")
	}
	if systemdLocation == "" {
		return nil, errors.New("Systemd location cannot be empty")
	}
	return &CreateSystemdService{
		SystemdCreateUtils:      systemdCreateUtils,
		BinaryDestinationFolder: binaryDestinationFolder,
		SystemdLocation:         systemdLocation,
		Logger:                  l,
		Writer:                  writer,
	}, nil
}

// Create the systemd service file.
func (css *CreateSystemdService) createSystemdServiceFile() error {
	// Create the template.
	tmpl, err := template.New(TEMPLATE_NAME).Parse(TEMPLATE_TEXT)
	if err != nil {
		return errors.Wrap(err, "Error parsing template")
	}

	// Prepare the template data.
	data := systemdInput{
		ServiceName:        SERVICE_NAME,
		ServiceDescription: SERVICE_DESCRIPTION,
		ServiceCommand:     css.BinaryDestinationFolder + "/" + SERVICE_COMMAND,
	}

	css.Logger.Debugf(
		"Template data: ServiceName %s, ServiceDescription %s, ServiceCommand %s",
		data.ServiceName, data.ServiceDescription, data.ServiceCommand,
	)

	// Execute the template to generate the service file.
	serviceFilePath := fmt.Sprintf(css.SystemdLocation+"/"+SYSTEMD_FILE, SERVICE_NAME)
	css.Logger.Debugf("Service file path %s", serviceFilePath)
	serviceFile, err := os.Create(serviceFilePath)
	if err != nil {
		return errors.Wrap(err, "Error creating service file")
	}
	defer serviceFile.Close()

	err = tmpl.Execute(serviceFile, data)
	if err != nil {
		return errors.Wrap(err, "Error executing template")
	}

	return nil
}

// Enable and start the systemd service.
func (css *CreateSystemdService) enableSystemdService() error {
	service := fmt.Sprintf(SYSTEMD_FILE, SERVICE_NAME)

	// TODO: Check if status is active and then stop the service.
	err := css.SystemdCreateUtils.ExecuteShellCommand("systemctl", []string{"status", service})
	if err == nil {
		css.Logger.Debugln("Status of service is active")
		err = css.SystemdCreateUtils.ExecuteShellCommand("systemctl", []string{"stop", service})
		if err != nil {
			return errors.Wrap(err, "Error stopping service")
		}
		css.Logger.Debugln("Stopped the service successfully")
	}
	css.Logger.Debugln("Checked status of service successfully")

	err = css.SystemdCreateUtils.ExecuteShellCommand("systemctl", []string{"daemon-reload"})
	if err != nil {
		return errors.Wrap(err, "Error reloading systemd daemon")
	}
	css.Logger.Debugln("Reloaded systemd daemon")
	err = css.SystemdCreateUtils.ExecuteShellCommand("systemctl", []string{"enable", service})
	if err != nil {
		return errors.Wrap(err, "Error enabling service")
	}
	css.Logger.Debugln("Enabled service successfully")
	err = css.SystemdCreateUtils.ExecuteShellCommand("systemctl", []string{"start", service})
	if err != nil {
		return errors.Wrap(err, "Error starting service")
	}
	css.Logger.Debugln("Started service successfully")
	return nil
}

func (css *CreateSystemdService) isSystemdEnabled() error {
	return css.SystemdCreateUtils.ExecuteShellCommand("systemctl", []string{"is-enabled", fmt.Sprintf(SYSTEMD_FILE, SERVICE_NAME)})
}

// Create and start the systemd service.
func (css *CreateSystemdService) Create() error {

	currentBinaryPath, err := css.SystemdCreateUtils.GetBinaryPath()
	css.Logger.Debugf("Current binary path %s", currentBinaryPath)
	if err != nil {
		return err
	}
	err = css.SystemdCreateUtils.SystemdRunning()
	if err != nil {
		return err
	}
	css.Logger.Debugln("Systemd is found on this system")

	err = css.isSystemdEnabled()
	if err == nil {
		err = css.enableSystemdService()
		if err != nil {
			return err
		}
	}
	css.Logger.Debugf("Service %s is not running on this system", SERVICE_NAME)

	fullBinaryDestination := filepath.Join(css.BinaryDestinationFolder, filepath.Base(currentBinaryPath))
	css.Logger.Debugf("Full binary destination path %s", fullBinaryDestination)
	err = css.SystemdCreateUtils.CreateDestinationAndCopy(currentBinaryPath, fullBinaryDestination)
	if err != nil {
		return err
	}

	css.Logger.Debugf("Binary copied from %s to %s\n", currentBinaryPath, fullBinaryDestination)

	err = css.createSystemdServiceFile()
	if err != nil {
		return err
	}

	err = css.enableSystemdService()
	if err != nil {
		return err
	}

	css.Writer.Printf("Service %s created successfully\n", SERVICE_NAME)

	return nil
}
