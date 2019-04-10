#!{{pkgPathFor "core/bash"}}/bin/bash

# Migrating A1 data to the A2 secrets service database
function migrate_A1_data() {
  echo "Migrating A1 secrets service tables"
  pg-helper -d migrate-tables-v2 "$A1_DBNAME" "$DBNAME" s_tags s_secrets s_secrets_tags

  if [ $? -ne 0 ]; then
      echo "Failed to migrate Automate 1 secrets service tables from $A1_DBNAME to $DBNAME"
      exit 1
  fi
}

# Migrating A2 compliance secrets service tables to the new A2 secrets service database
function migrate_A2_data() {
  pg-helper -d migrate-tables-v2 "$COMPLIANCE_DBNAME" "$DBNAME" s_tags s_secrets s_secrets_tags \
    --fail-if-src-missing

  errcode="$?"
  if [ $errcode -eq 0 ]; then
      echo "Migrated secrets service tables from $COMPLIANCE_DBNAME"
      return 0
  elif [ $errcode -eq 10 ]; then
      # 10 means the source database did not exist
      echo "$COMPLIANCE_DBNAME does not exist. No tables migrated"
      return 1
  else
      echo "Failed to migrate Automate 2 secrets service tables from $COMPLIANCE_DBNAME to $DBNAME"
      exit 1
  fi
}

function trigger_migrations() {
  migrate_A2_data
  if [[ $? -ne 0 ]]; then
    migrate_A1_data
  fi
}
