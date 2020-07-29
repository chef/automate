package chef

type RequiredRecipeService struct {
	client *Client
}

// RequireRecipe the text of the required recipe.
type RequiredRecipe string

// RequiredRecipe gets the optional required_runlist value.
//
// https://docs.chef.io/api_chef_server/
// 200 - required_recipe enabled = true && required_recipe path specified, returns the recipe
// 404   required_recipe enabled = false
func (e *RequiredRecipeService) Get() (data RequiredRecipe, err error) {
	var getdata string
	err = e.client.magicRequestDecoder("GET", "required_recipe", nil, &getdata)
	data = RequiredRecipe(getdata)
	return
}
