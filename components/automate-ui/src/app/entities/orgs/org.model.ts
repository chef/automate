export interface Org {
  id: string;
  server_id: string;
  name: string;
  admin_user: string;
  projects?: string[];
}

export interface UploadFile {
  server_id: string;
  file: string;
  // formData: any;
}