import { Entity } from '../entities';

export class UserPermEntity implements Entity {
  // The id is the endpoint path
  public id: string;
  // These correspond to automate-gateway/api/authz/response/authz.proto
  public get: boolean;
  public put: boolean;
  public post: boolean;
  public delete: boolean;
  public patch: boolean;

  constructor({get, put, post, delete: del, patch}, id) {
    this.id = id;
    this.get = get;
    this.put = put;
    this.post = post;
    this.delete = del;
    this.patch = patch;
  }

  public static create([endpointPath, permJson]): UserPermEntity {
    return new UserPermEntity(permJson, endpointPath);
  }
}
