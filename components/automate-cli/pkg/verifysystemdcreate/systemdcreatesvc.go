package verifysystemdcreate

import (
	"fmt"
	"os"
	"path/filepath"
	"text/template"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
)

const (
	SERVICE_NAME          = "automate-verify"
	SERVICE_DESCRIPTION   = "Service for automating verification"
	SERVICE_COMMAND       = "chef-automate verify serve"
	SYSTEMD_FILE          = "%s.service"
	ENABLE_SYSTEMD_SCRIPT = "systemctl daemon-reload && systemctl enable %[1]v.service && systemctl start %[1]v.service"
	TEMPLATE_NAME         = "systemd-service"
	TEMPLATE_TEXT         = `[Unit]
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
	CreateDestinationAndCopyFunc func(binarySrcPath, binaryDestPath string) error
	ExecuteShellCommandFunc      func(command string) error
	BinaryDestinationFolder      string
	CurrentBinaryPath            string
	SystemdLocation              string
	Writer                       *cli.Writer
}

func NewCreateSystemdService(
	createDestinationAndCopy func(binarySrcPath, binaryDestPath string) error,
	executeShellCommand func(command string) error,
	binaryDestinationFolder string,
	systemdLocation string,
	currentBinaryPath string,
	writer *cli.Writer) (*CreateSystemdService, error) {
	if binaryDestinationFolder == "" {
		return nil, errors.New("binary destination folder cannot be empty")
	}
	if systemdLocation == "" {
		return nil, errors.New("systemd location cannot be empty")
	}
	return &CreateSystemdService{
		CreateDestinationAndCopyFunc: createDestinationAndCopy,
		ExecuteShellCommandFunc:      executeShellCommand,
		BinaryDestinationFolder:      binaryDestinationFolder,
		CurrentBinaryPath:            currentBinaryPath,
		SystemdLocation:              systemdLocation,
		Writer:                       writer,
	}, nil
}

// type CreateSystemdService struct{}

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

	// Execute the template to generate the service file.
	serviceFilePath := fmt.Sprintf(css.SystemdLocation+"/"+SYSTEMD_FILE, SERVICE_NAME)
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
	systemctlCmd := fmt.Sprintf(ENABLE_SYSTEMD_SCRIPT, SERVICE_NAME)
	err := css.ExecuteShellCommandFunc(systemctlCmd)
	if err != nil {
		return errors.Wrap(err, "Error enabling service")
	}
	return nil
}

// Create the systemd service.
func (css *CreateSystemdService) Create() error {
	fullBinaryDestination := filepath.Join(css.BinaryDestinationFolder, filepath.Base(css.CurrentBinaryPath))

	err := css.CreateDestinationAndCopyFunc(css.CurrentBinaryPath, fullBinaryDestination)
	if err != nil {
		return err
	}

	css.Writer.Printf("Binary copied to %s\n", fullBinaryDestination)

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
