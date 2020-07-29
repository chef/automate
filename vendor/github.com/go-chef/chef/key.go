package chef

import ()

//For all API date inputs and outputs, we will be strictly using ISO8601 in
//UTC format, which looks like `YYYY-MM-DDThh:mm:ssZ` where `T` is the character
//`T` that separates years, months, days from hours, minutes, seconds and `Z`
//is the character `Z` to denote UTC time as is standard in ISO8601. We should
//strictly validate all date inputs to webmachine / our API to match the above format, and
//all dates should be outputted in said format.
// The exception to this is that "infinity" is a valid date input and can be parsed by all base functions
// and properly inserted into the database with a little massaging (also in base modules).

type ChefKey struct {
	Name           string `json:"name"`
	PublicKey      string `json:"public_key"`
	ExpirationDate string `json:"expiration_date"`
	Uri            string `json:"uri,omitempty"`
	PrivateKey     string `json:"private_key"`
}

type AccessKey struct {
	Name           string `json:"name,omitempty"`
	PublicKey      string `json:"public_key,omitempty"`
	ExpirationDate string `json:"expiration_date,omitempty"`
}

type KeyItem struct {
	Name    string `json:"name,omitempty"`
	Uri     string `json:"uri,omitempty"`
	Expired bool   `json:"expired,omitempty"`
}
