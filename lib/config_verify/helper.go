package config_verify

import (
	"container/list"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func validateRequiredNumberField(value string, fieldName string, errorList *list.List) {
	if len(value) > 0 {
		if _, err := strconv.Atoi(value); err != nil {
			errorList.PushBack(fmt.Sprintf("Invalid %s: %s", fieldName, value))
		}
	} else {
		errorList.PushBack(fmt.Sprintf("Invalid or empty %s", fieldName))
	}
}

func validateRequiredBooleanField(value interface{}, fieldName string, errorList *list.List) {
	_, ok := value.(bool)
	if !ok {
		errorList.PushBack(fmt.Sprintf("Invalid %s: %s", fieldName, value))
	}
}

func validateRequiredPathField(value string, fieldName string, errorList *list.List) {
	if len(value) > 0 {
		if _, err := os.Stat(value); err != nil {
			if errors.Is(err, os.ErrNotExist) {
				errorList.PushBack(fmt.Sprintf("Invalid %s: %s (path does not exist)", fieldName, value))
			} else {
				errorList.PushBack(fmt.Sprintf("Invalid %s: %s (%v)", fieldName, value, err))
			}
		}
	} else {
		errorList.PushBack(fmt.Sprintf("Invalid or empty %s", fieldName))
	}
}

func validateRequiredStringTypeField(value interface{}, fieldName string, errorList *list.List) {
	strValue, ok := value.(string)
	if !ok || len(strings.TrimSpace(strValue)) == 0 {
		errorList.PushBack(fmt.Sprintf("Invalid or empty %s", fieldName))
	}
}

func validateRequiredStringListField(value []string, fieldName string, errorList *list.List) {
	if len(value) < 1 {
		errorList.PushBack(fmt.Sprintf("Invalid or empty %s", fieldName))
	}
}

func validateBackupMount(mount string, errorList *list.List) {
	if len(mount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	} else if mount != "/mnt/automate_backups" {
		errorList.PushBack("backup_mount has to be /mnt/automate_backups")
	}
}

func getSingleErrorFromList(errorList *list.List) error {
	if errorList.Len() > 0 {
		var errorMsgs []string
		for e := errorList.Front(); e != nil; e = e.Next() {
			errorMsgs = append(errorMsgs, e.Value.(string))
		}
		return fmt.Errorf(strings.Join(errorMsgs, "\n"))
	}
	return nil
}

func validateFQDN(value string, errorList *list.List) {
	if len(value) < 1 {
		errorList.PushBack("Invalid or empty FQDN")
		return
	}

	if strings.Contains(value, " ") {
		errorList.PushBack("domain name cannot contain spaces")
		return
	}

	// Check for "http://" or "https://" in the FQDN value
	if strings.HasPrefix(value, "http://") || strings.HasPrefix(value, "https://") {
		errorList.PushBack("fqdn should not include protocol (http:// or https://)")
		return
	}

	// Regular expression to validate FQDN
	regex := `^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$`
	match, _ := regexp.MatchString(regex, value)
	if !match {
		errorList.PushBack("Invalid FQDN format")
	}
}
