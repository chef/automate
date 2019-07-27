package commands

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/lib/license"
)

func init() {
	RootCmd.AddCommand(printCmd)
}

var printCmd = &cobra.Command{
	Use:   "pretty-print LICENSE_STRING_OR_FILE",
	Short: "Decode and pretty-print the given Automate license",
	Args:  cobra.ExactArgs(1),
	Run:   runPrintCmd,
}

func runPrintCmd(cmd *cobra.Command, args []string) {
	publicKeys := keys.LoadPublicKeys(keys.BuiltinKeyData)
	licData, err := maybeFromFile(args[0])
	if err != nil {
		log.WithError(err).Fatal("failed to read user-provided license data")
	}

	sha, err := license.GetKeySha256(licData)
	if err != nil {
		log.WithError(err).Fatal("failed to get SHA256 from license data")
	}

	lic, err := license.Read(licData, publicKeys[sha])
	if err != nil {
		log.WithError(err).Fatal("failed to read license data")
	}

	// Use json for pretty printing the struct
	bytes, err := json.MarshalIndent(lic, "", "  ")
	if err != nil {
		log.WithError(err).Fatal("could not marshal license as json")
	}

	fmt.Println(string(bytes))
}

// maybeFromFile provides a bit of user-friendliness by reading the
// token from disk if the passed string happens to be a file on disk
// that exists.
func maybeFromFile(maybeToken string) (string, error) {
	_, err := os.Stat(maybeToken)
	if err == nil {
		log.Debugf("Reading token data from file: %s\n", maybeToken)
		data, err := ioutil.ReadFile(maybeToken)
		if err != nil {
			log.WithError(err).Fatal("Reading token data from file failed")
		}
		strippedData := strings.TrimSpace(string(data))
		return strippedData, nil
	}
	log.Debug("No such file appears on disk, assuming that the argument is your license key content")
	return maybeToken, nil
}
