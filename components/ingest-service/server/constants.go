package server

const (
	//Stages
	GET_ALIASES      string = "get_aliases"
	SRC_TO_TEMP      string = "create_src_to_temp"
	REINDEX_SRC_TEMP string = "reindex_src_temp"
	DELETE_SRC       string = "delete_src"
	TEMP_TO_SRC      string = "create_temp_to_src"
	REINDEX_TEMP_SRC string = "reindex_temp_src"
	CREATE_ALIASES   string = "create_aliases"
	DELETE_TEMP      string = "delete_temp"

	//Status
	STATUS_RUNNING   string = "running"
	STATUS_COMPLETED string = "completed"
	STATUS_FAILED    string = "failed"
)
