import { Injectable } from "@angular/core";
export interface KVData {
  key?: string;
  value?: string;
}

export interface NodeCredential {
  id?: string;
  name: string;
  type: NodeCredentialTypes;
  last_modified?: string;
  tags: Array<KVData>;
  data: Array<KVData>;
}

export enum NodeCredentialTypes {
  None = 'none',
  SSH = 'ssh',
  WinRM = 'winrm',
  Sudo = 'sudo'
}

export interface NodeCredentialsList {
  items: NodeCredential[];
  total: number;
  page: number;
  per_page: number;
  sort: string;
  order: string;
}

export interface NodeObject {
  id: string;
  name: string;
  type: NodeCredentialTypes;
  ssh: {username: string, password: string, key: string};
  winrm: {username: string, password: string};
  sudo: {password: string, options: string};
}

@Injectable()
export class SaveNodeCredential {

  getNodeCredentialCreate(data: NodeObject): NodeCredential {

    const credBody = {
      id: data.id,
      name: data.name,
      type: data.type,
      data: [],
      tags: []
    };

    switch (data.type) {
      case NodeCredentialTypes.SSH: {
        credBody.data.push({ key: 'username', value: data.ssh.username });
        if (data.ssh.password !== '') {
          credBody.data.push({ key: 'password', value: data.ssh.password });
        }
        if (data.ssh.key !== '') {
          credBody.data.push({ key: 'key', value: data.ssh.key });
        }
        break;
      }
      case NodeCredentialTypes.WinRM: {
        credBody.data.push(
          { key: 'username', value: data.winrm.username },
          { key: 'password', value: data.winrm.password }
        );
        break;
      }
      case NodeCredentialTypes.Sudo: {
        credBody.data.push(
          { key: 'password', value: data.sudo.password },
          { key: 'options', value: data.sudo.options }
        );
        break;
      }
      default: {
        throw new Error(`Invalid credential type: ${data.type}`);
      }
    }
    return credBody;
  }

}
