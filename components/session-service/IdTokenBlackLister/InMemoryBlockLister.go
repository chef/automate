package IdTokenBlackLister

type InMemoryBlackLister struct {
	blackListedTokens map[string]bool
}

func NewInMemoryBlackLister() InMemoryBlackLister {
	return InMemoryBlackLister{
		blackListedTokens: make(map[string]bool),
	}
}

func (a InMemoryBlackLister) InsertToBlackListedStore(idToken string) error {
	a.blackListedTokens[idToken] = true
	return nil
}

func (a InMemoryBlackLister) IsIdTokenBlacklisted(idToken string) (bool, error) {
	_, ok := a.blackListedTokens[idToken]
	return ok, nil
}
