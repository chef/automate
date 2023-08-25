package constants

const (
	SUDO_PASSWORD_CMD                           = `echo "%s" | sudo -S ls -l`
	PEM_FILE_NAME                               = "private_key.pem"
	SSH_USER_SUCCESS_TITLE                      = "SSH user accessible"
	SSH_USER_SUCCESS_MESSAGE                    = "SSH user is accessible for the node: "
	SUDO_PASSWORD_TITLE                         = "Sudo password valid"
	SUDO_PASSWORD_SUCCESS_MESSAGE               = "SSH user sudo password is valid for the node: "
	SSH_USER_FAILURE_TITLE                      = "SSH user unaccessible"
	SSH_USER_ERROR_MESSAGE                      = "SSH user is unaccessible for the node with IP: "
	SSH_USER_RESOLUTION_MESSAGE                 = "Give SSH access to the user with the given key on the node: "
	SUDO_PASSWORD_FAILURE_TITLE                 = "Sudo password invalid"
	SUDO_PASSWORD_ERROR_MESSAGE                 = "SSH user sudo password is invalid for the node with IP: "
	SUDO_PASSWORD_FAILURE_RESOLUTION_MESSAGE    = "Ensure you have provided the correct sudo password and the user has sudo access on the node: "
	SUDO_PASSWORD_CONNECTION_ERROR_MESSAGE      = "SSH connection failed on the node so unable to check the sudo password for the node with IP: "
	SUDO_PASSWORD_CONNECTION_RESOLUTION_MESSAGE = "Ensure the correct credentials are provided for the SSH connection of node with IP: "
)
