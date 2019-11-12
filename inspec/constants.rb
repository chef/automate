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
DEFAULT_POLICY_IDS = [
    "administrator-access",
    "editor-access",
    "viewer-access",
    "ingest-access"
]
UNASSIGNED_PROJECT_ID = '(unassigned)'
UNASSIGNED_PROJECT_NAME = UNASSIGNED_PROJECT_ID
