package IdTokenBlackLister

// IdTokenBlackLister interface exposes functions to be used to
// blacklist and validate idtokens
type IdTokenBlackLister interface {
	// InsertToBlackListedStore inserts the token as blacklisted
	// Being in blacklisted makes sure that the token cannot be used further
	InsertToBlackListedStore(string) error

	// IsIdTokenBlacklisted looks up to the blacklisted store
	// to check if the token exists.
	IsIdTokenBlacklisted(string) (bool, error)
}
