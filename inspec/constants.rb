# global constants used in several different controls

TIMESTAMP = Time.now.utc.to_i

ADMIN_USER_ID = 'admin'
DEFAULT_ROLE_IDS = [
    "owner",
    "editor",
    "viewer",
    "ingest",
    "project-owner"
]

ADMIN_POLICY_ID = "administrator-access"
EDITOR_POLICY_ID = "editor-access"
VIEWER_POLICY_ID = "viewer-access"
INGEST_POLICY_ID = "ingest-access"

UNASSIGNED_PROJECT_ID = '(unassigned)'
UNASSIGNED_PROJECT_NAME = UNASSIGNED_PROJECT_ID

MIGRATED_TOKEN_ID = "inspec-migrated-admin-token"
MIGRATED_TOKEN = "o2Q8iCbyvxX8W4393BvyyqmL-ug="
MIGRATED_TEAM_VIEWER = "inspec-migrated-test-non-admin"
