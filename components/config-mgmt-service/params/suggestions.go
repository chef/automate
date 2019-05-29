package params

// For suggestions we use engram type mappings to support autocomplete functionality.

type SuggestionType int

// Ended these constants with sug so that they do not get mixed up with other parameter validation constants
const (
	PlatformSug SuggestionType = iota
	NameSug
	EnvironmentSug
	PolicyGroupSug
	PolicyNameSug
	PolicyRevisionSug
	CookbooksSug
	RecipesSug
	ResourceNamesSug
	AttributesSug
	RolesSug
	ChefVersionSug
	ChefTagsSug
	ErrorSug
)

var suggestionType = map[string]SuggestionType{
	"platform":        PlatformSug,
	"name":            NameSug,
	"environment":     EnvironmentSug,
	"policy_group":    PolicyGroupSug,
	"policy_name":     PolicyNameSug,
	"policy_revision": PolicyRevisionSug,
	"cookbook":        CookbooksSug,
	"recipe":          RecipesSug,
	"resource_name":   ResourceNamesSug,
	"attribute":       AttributesSug,
	"role":            RolesSug,
	"chef_version":    ChefVersionSug,
	"chef_tags":       ChefTagsSug,
	"error":           ErrorSug,
}

func ValidSuggestionParam(field string) bool {
	if _, ok := suggestionType[field]; ok {
		return true
	}
	return false
}
