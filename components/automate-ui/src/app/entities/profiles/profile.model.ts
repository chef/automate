export interface Profile {
  id: string;
  name: string;
  title: string;
  copyright: string;
  copyright_email: string;
  license: string;
  summary: string;
  version: string;
  owner: string;
  supports: any[];
  depends: any[];
  sha256: string;
  groups: any[];
  controls: any[];
  attributes: any[];
  latest_version: string;
}
