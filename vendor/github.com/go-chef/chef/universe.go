package chef

type UniverseService struct {
	client *Client
}

// Universe represents the body of the returned information.
type Universe struct {
	Books map[string]UniverseBook
}

type UniverseBook struct {
	Versions map[string]UniverseVersion
}

type UniverseVersion struct {
	LocationPath string
	LocationType string
	Dependencies map[string]string
}

// Universe gets available cookbook version information.
//
// https://docs.chef.io/api_chef_server.html#universe
func (e *UniverseService) Get() (universe Universe, err error) {
	var data map[string]interface{}
	err = e.client.magicRequestDecoder("GET", "universe", nil, &data)
	unpackUniverse(&universe, &data)
	return
}

func unpackUniverse(universe *Universe, data *map[string]interface{}) {
	(*universe).Books = make(map[string]UniverseBook)
	for bookn, versions := range *data {
		ub := UniverseBook{}
		ub.Versions = make(map[string]UniverseVersion)
		switch versions.(type) {
		case map[string]interface{}:
			for vname, version := range versions.(map[string]interface{}) {
				uv := UniverseVersion{}
				switch version.(type) {
				case map[string]interface{}:
					for aname, attr := range version.(map[string]interface{}) {
						deps := make(map[string]string)
						switch aname {
						case "dependencies":
							for dname, dep := range attr.(map[string]interface{}) {
								switch dep.(type) {
								case string:
									deps[dname] = dep.(string)
								default:
								}
							}
							uv.Dependencies = deps
						case "location_path":
							uv.LocationPath = attr.(string)
						case "location_type":
							uv.LocationType = attr.(string)
						}
					}
				default:
				}
				ub.Versions[vname] = uv
			}
		default:
		}
		(*universe).Books[bookn] = ub
	}
	return
}
