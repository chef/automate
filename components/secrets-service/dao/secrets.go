package dao

import (
	"encoding/base64"
	"encoding/json"
	"time"

	"github.com/pkg/errors"
	logs "github.com/sirupsen/logrus"

	"fmt"

	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"io"

	"github.com/golang/protobuf/ptypes"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/secrets-service/utils"
	"github.com/chef/automate/lib/stringutils"
)

const selectSecrets = `
SELECT
  s.id,
  s.name,
  s.type,
  s.last_modified,
  COALESCE(('[' || string_agg('{"key":"' || t.key || '"' || ',"value": "' || t.value || '"}', ',') || ']'),
           '[]') :: JSON AS tags,
  COUNT(*)
  OVER ()                AS total_count
FROM s_secrets s
  LEFT JOIN s_secrets_tags st ON (s.id = st.secret_id)
	LEFT JOIN s_tags t ON (t.id = st.tag_id)
%s
GROUP BY s.id
ORDER BY %s %s
LIMIT $1
OFFSET $2;
`

const selectSecret = `
SELECT
  s.id,
  s.name,
  s.type,
  s.last_modified,
  s.data,
  COALESCE(('[' || string_agg('{"key":"' || t.key || '"' || ',"value": "' || t.value || '"}', ',') || ']'),
           '[]') :: JSON AS tags
FROM s_secrets s
  LEFT JOIN s_secrets_tags st ON s.id = st.secret_id
  LEFT JOIN s_tags t ON t.id = st.tag_id
WHERE s.id = $1
GROUP BY s.id;
`

const deleteSecretTags = `
DELETE FROM s_tags
WHERE id IN (SELECT tag_id
             FROM s_secrets_tags
             WHERE secret_id = $1);
`

const deleteNodesSecrets = `
DELETE FROM s_secrets
WHERE id=$1;
`

const secretExists = `
SELECT exists(SELECT 1
              FROM s_secrets
              WHERE id = $1) AS "exists"
`

var secretsSortFields = map[string]string{
	"name":          "LOWER(s.name)",
	"type":          "LOWER(s.type)",
	"last_modified": "s.last_modified",
}

// secret used to put in db (insert, delete, update)
type secret struct {
	ID           string    `db:"id"`
	Name         string    `db:"name"`
	Type         string    `db:"type"`
	LastModified time.Time `db:"last_modified"`
	Data         string    `db:"data"`
}

// secretSelect used to read from db
type secretSelect struct {
	ID           string          `db:"id"`
	Name         string          `db:"name"`
	Type         string          `db:"type"`
	LastModified time.Time       `db:"last_modified"`
	Tags         json.RawMessage `db:"tags"`
	Data         string          `db:"data"`
	TotalCount   int64           `db:"total_count"`
}

// SecretTag used only to (de)serialize database access
type SecretTag struct {
	SecretID string `db:"secret_id"`
	TagID    string `db:"tag_id"`
}

// Ping the database to in sure it connection is working.
func (secretsDb *DB) Ping() error {
	return secretsDb.Db.Ping()
}

func (secretsDb *DB) toDBSecret(inSecret *secrets.Secret) (*secret, error) {
	newSecret := secret{}
	newSecret.ID = inSecret.Id
	newSecret.Name = inSecret.Name
	newSecret.Type = inSecret.Type
	newSecret.LastModified = time.Now().UTC()

	jsonData, err := KeyValueToRawMap(inSecret.Data)
	if err != nil {
		return nil, errors.Wrap(err, "toDBSecret error")
	}
	s, err := encryptString(string(jsonData), secretsDb.SecretsKey)
	if err != nil {
		return nil, errors.Wrap(err, "toDBSecret error")
	}

	encodedEncryptedData := base64.StdEncoding.EncodeToString([]byte(*s))
	newSecret.Data = encodedEncryptedData

	return &newSecret, nil
}

func (secretsDb *DB) fromDBSelectSecret(inSecret *secretSelect) (*secrets.Secret, error) {
	newSecret := secrets.Secret{}
	newSecret.Id = inSecret.ID
	newSecret.Name = inSecret.Name
	newSecret.Type = inSecret.Type
	newSecret.LastModified, _ = ptypes.TimestampProto(inSecret.LastModified)
	var tags []*secrets.Kv
	err := json.Unmarshal(inSecret.Tags, &tags)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBSelectSecret error unmarshalling tags")
	}
	newSecret.Tags = tags
	var data []*secrets.Kv
	if inSecret.Data != "" {
		decodedEncryptedString, err := base64.StdEncoding.DecodeString(inSecret.Data)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBSelectSecret decode error")
		}
		s, err := decryptString(string(decodedEncryptedString), secretsDb.SecretsKey)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBSelectSecret error decrypting string")
		}

		data, err = RawMapToKeyValue(json.RawMessage(*s))
		if err != nil {
			return nil, errors.Wrap(err, "fromDBSelectSecret error processing data")
		}
		newSecret.Data = data
	}

	return &newSecret, nil
}

func encryptString(plaintext string, key string) (*string, error) {
	c, err := aes.NewCipher([]byte(key))
	if err != nil {
		return nil, errors.Wrap(err, "encrypt string error")
	}

	gcm, err := cipher.NewGCM(c)
	if err != nil {
		return nil, errors.Wrap(err, "encrypt string error")
	}

	nonce := make([]byte, gcm.NonceSize())
	if _, err = io.ReadFull(rand.Reader, nonce); err != nil {
		return nil, errors.Wrap(err, "encrypt string error")
	}

	enc := string(gcm.Seal(nonce, nonce, []byte(plaintext), nil))
	return &enc, nil
}

func decryptString(ciphertext string, key string) (*string, error) {
	c, err := aes.NewCipher([]byte(key))
	if err != nil {
		return nil, errors.Wrap(err, "decrypt string error")
	}

	gcm, err := cipher.NewGCM(c)
	if err != nil {
		return nil, errors.Wrap(err, "decrypt string error")
	}

	nonceSize := gcm.NonceSize()
	if len(ciphertext) < nonceSize {
		return nil, errors.Wrap(err, "decrypt string error: ciphertext too short")
	}

	nonce, ciphertext := ciphertext[:nonceSize], ciphertext[nonceSize:]
	decByte, err := gcm.Open(nil, []byte(nonce), []byte(ciphertext), nil)
	decString := string(decByte)

	return &decString, err
}

//UpdateSecret updates a secret in the db with input values
func (secretsDb *DB) UpdateSecret(inSecret *secrets.Secret) (count int64, err error) {
	secret, err := secretsDb.toDBSecret(inSecret)
	if err != nil {
		err = errors.Wrap(err, fmt.Sprintf("UpdateSecret: unable to translate secret to db struct %+v", inSecret))
		return
	}
	secret.LastModified = timeNowRef()

	err = Transact(secretsDb, func(tx *DBTrans) error {
		//get list of tags from current secret
		_, err := tx.Exec(deleteSecretTags, secret.ID)
		if err != nil {
			return errors.Wrap(err, "unable to delete secret tags")
		}

		tags, err := tx.addSecretTags(inSecret.Tags)
		if err != nil {
			return errors.Wrap(err, "unable to add secret tags")
		}

		err = tx.tagSecret(inSecret.Id, tags)
		if err != nil {
			return errors.Wrap(err, "unable to tag secret")
		}

		_, err = tx.Update(secret)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("UpdateSecret: unable to update secret in db %+v", inSecret))
		}
		return nil
	})

	return
}

// AddSecret adds a secret to the db
func (secretsDb *DB) AddSecret(inSecret *secrets.Secret) (string, error) {
	secret, err := secretsDb.toDBSecret(inSecret)
	if err != nil {
		return "", errors.Wrap(err, "AddSecret: unable to translate secret into db struct")
	}
	secret.ID = createUUID()
	secret.LastModified = timeNowRef()

	err = Transact(secretsDb, func(tx *DBTrans) error {
		if err = tx.Insert(secret); err != nil {
			return errors.Wrap(err, "AddSecret: unable to insert secret")
		}

		tags, err := tx.addSecretTags(inSecret.Tags)
		if err != nil {
			return errors.Wrap(err, "AddSecret: unable to add secret tags to db")
		}

		err = tx.tagSecret(secret.ID, tags)
		if err != nil {
			return errors.Wrap(err, "AddSecret: unable to tag the secret")
		}
		return nil
	})

	return secret.ID, err
}

// SecretExists is used to tell us if the secret exists in the db
func (secretsDb *DB) SecretExists(id string) (exists bool, err error) {
	err = secretsDb.SelectOne(&exists, secretExists, id)
	return
}

// GetSecrets retrieves all secrets from the db matching input query params
func (secretsDb *DB) GetSecrets(sortField string, insortOrder secrets.Query_OrderType, pageNr int32,
	perPage int32, filters []*secrets.Filter) ([]*secrets.Secret, int64, error) {
	var sortOrder string
	sortField = valueOrDefaultStr(sortField, "name")
	if insortOrder == 1 {
		sortOrder = "desc"
	} else {
		sortOrder = "asc"
	}
	pageNr = valueOrDefaultInt(pageNr, 1) - 1 //offset
	perPage = valueOrDefaultInt(perPage, 100) //limit

	if secretsSortFields[sortField] == "" {
		return nil, 0, &utils.InvalidError{Msg: fmt.Sprintf("Invalid sort field, valid ones are: %v", getMapKeys(secretsSortFields))}
	}
	if !stringutils.SliceContains(validOrderFields, sortOrder) {
		return nil, 0, &utils.InvalidError{Msg: fmt.Sprintf("Invalid order, valid ones are: %v", validOrderFields)}
	}
	err := validateSecretFilters(filters)
	if err != nil {
		return nil, 0, errors.Wrapf(err, "GetSecrets error validating secrets filters")
	}
	whereFilter, err := buildWhereFilter(filters, "s", secretsFilterField)
	if err != nil {
		return nil, 0, errors.Wrap(err, "GetSecrets error building where filter")
	}
	secrets := make([]*secrets.Secret, 0)
	var secretsDaos []*secretSelect
	logs.Debugf("SQL: %s", fmt.Sprintf(selectSecrets, whereFilter, secretsSortFields[sortField], sortOrder))
	_, err = secretsDb.Select(&secretsDaos, fmt.Sprintf(selectSecrets, whereFilter, secretsSortFields[sortField], sortOrder), perPage, pageNr*perPage)
	if err != nil {
		return secrets, 0, errors.Wrap(err, "getsecrets: error getting secrets")
	}

	totalCount := int64(0)
	if len(secretsDaos) > 0 {
		totalCount = secretsDaos[0].TotalCount
	}

	for _, secretDao := range secretsDaos {
		secret, err := secretsDb.fromDBSelectSecret(secretDao)
		if err != nil {
			return secrets, totalCount, errors.Wrap(err, "getsecrets: error translating secret")
		}
		secrets = append(secrets, secret)
	}
	return secrets, totalCount, nil
}

var secretsFilterField = map[string]string{
	"type": "type",
}

func validateSecretFilters(filters []*secrets.Filter) error {
	for _, filter := range filters {
		switch filter.Key {
		case "type":
			for _, item := range filter.Values {
				if !isValidSecretType(item) {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid type filter: %s. type must be one of the following: ssh, winrm, sudo, aws, azure, service_now", item)}
				}
			}
		}
	}
	return nil
}

func isValidSecretType(secretType string) bool {
	switch secretType {
	case "ssh", "winrm", "sudo", "aws", "azure", "service_now":
		return true
	default:
		return false
	}
}

// GetSecret is used to read a secret from the db
func (secretsDb *DB) GetSecret(id string) (*secrets.Secret, error) {
	var secret secretSelect
	err := secretsDb.SelectOne(&secret, selectSecret, id)
	var newSecret *secrets.Secret
	if err != nil {
		return nil, utils.ProcessSQLNotFound(err, id, "GetSecret")
	}

	newSecret, err = secretsDb.fromDBSelectSecret(&secret)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("GetSecret: unable to translate secret from db struct %+s", id))
	}
	return newSecret, nil
}

// DeleteSecret deletes a secret from the db
func (secretsDb *DB) DeleteSecret(id string) (int64, error) {
	var secret secret
	var count int64

	err := Transact(secretsDb, func(tx *DBTrans) error {
		secret.ID = id

		_, err := tx.Exec(deleteSecretTags, id)
		if err != nil {
			return utils.ProcessSQLNotFound(err, id, "DeleteSecret")
		}

		_, err = tx.Exec(deleteNodesSecrets, id)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("DeleteSecret: unable to delete NodesSecret for secret %+s", id))
		}

		count, err = tx.Delete(&secret)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("DeleteSecret: unable to delete secret from db %+s", id))
		}
		return nil
	})

	return count, err
}
