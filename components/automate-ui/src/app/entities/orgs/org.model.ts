export interface Org {
  id: string;
  server_id: string;
  name: string;
  admin_user: string;
  projects?: string[];
}

export interface UploadFile {
  formData: FormData;
}
<<<<<<< HEAD

export interface PreviewData {
  migration_id: string;
  staged_data: {
    orgs_to_migrate: number;
    orgs_to_skip: number;
    orgs_to_update: number;
    orgs_to_delete: number;
    users: []
  };
}

export interface User {
  username: string;
  email: string;
  display_name: string;
  first_name: string;
  last_name: string;
  middle_name: string;
  automate_username: string;
  connector: string;
  is_conflicting: boolean;
  is_admin: boolean;
}
=======
>>>>>>> d5e176b0b (Stalwart 32 upload slider functionality (#6654))
