package genconfig

type IConfig interface {
	Toml() (tomlBytes []byte, err error)
	Prompts() (err error)
}
