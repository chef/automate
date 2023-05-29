package models

type ExternalPgConnectionDetails struct {
	Title         string `json:"title"`
	Passed        bool   `json:"passed"`
	Status        string `json:"status"`
	SuccessMsg    string `json:"success_msg"`
	ErrorMsg      string `json:"error_msg"`
	ResolutionMsg string `json:"resolution_msg"`
}

type ExternalPgRequest struct {
	PostgresqlInstanceUrl       string `json:"postgresql_instance_url"`
	PostgresqlInstancePort      string `json:"postgresql_instance_port"`
	PostgresqlSuperUserUserName string `json:"postgresql_superuser_username"`
	PostgresqlSuperUserPassword string `json:"postgresql_superuser_password"`
	PostgresqlDbUserUserName    string `json:"postgresql_dbuser_username"`
	PostgresqlDbUserPassword    string `json:"postgresql_dbuser_password"`
	PostgresqlRootCert          string `json:"postgresql_root_cert"`
}

type ExternalPgResponse struct {
	Passed bool                          `json:"passed"`
	Checks []ExternalPgConnectionDetails `json:"checks"`
}
