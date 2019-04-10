#!{{pkgPathFor "core/bash"}}/bin/bash

# Migrating A1 data to the A2 nodemanager service database
function migrate_A1_data() {
  echo "Migrating A1 nodemanager service tables"
  pg-helper -d migrate-tables-v2 "$A1_DBNAME" "$DBNAME" node_managers tags nodes nodes_secrets nodes_tags 
  
  if [ $? -ne 0 ]; then
      echo "Failed to migrate Automate 1 nodemanager service tables from $A1_DBNAME to $DBNAME"
      exit 1
  fi
}

# Migrating A2 compliance nodemanager service tables to the new A2 nodemanager service database
function migrate_A2_data() {
  pg-helper -d migrate-tables-v2 "$COMPLIANCE_DBNAME" "$DBNAME" node_managers tags nodes nodes_secrets nodes_tags node_managers_nodes

  errcode="$?"
  if [ $errcode -eq 0 ]; then
      echo "Migrated nodemanager service tables from $COMPLIANCE_DBNAME"
      return 0
  elif [ $errcode -eq 10 ]; then
      # 10 means the source database did not exist
      echo "$COMPLIANCE_DBNAME does not exist. No tables migrated"
      return 1
  else
      echo "Failed to migrate Automate 2 nodemanager service tables from $COMPLIANCE_DBNAME to $DBNAME"
      exit 1
  fi
}

function trigger_migrations() {
  migrate_A2_data
  if [[ $? -ne 0 ]]; then
    migrate_A1_data
  fi
}