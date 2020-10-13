import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment as env } from 'environments/environment';
import { NodeCredential } from './node-credential.model';
import { CreateNodeCredentialPayload,
  NodeCredentialsSearchPayload } from './node-credential.actions';
import { Observable } from 'rxjs';
import { mapKeys, snakeCase } from 'lodash/fp';

export interface NodeCredentialResponse {
  nodeCredential: NodeCredential;
}
export interface NodeCredentialsResponse {
  nodeCredentials: NodeCredential;
}

export interface NodeCredentialsSearchResponse {
  secrets: NodeCredential[];
  total: number;
}


@Injectable()
export class NodeCredentialRequests {

  constructor(private http: HttpClient) { }

  public getNodeCredentials(payload: NodeCredentialsSearchPayload):
  Observable<NodeCredentialsSearchResponse> {
    return this.http.post<NodeCredentialsSearchResponse>(
      `${env.secrets_url}/search`, mapKeys(snakeCase, payload));  }

  public getNodeCredential(id)  {
    return this.http.get<NodeCredential>(`${env.secrets_url}/id/${id}`);
  }

  public createNodeCredential(credentialData: CreateNodeCredentialPayload):
  Observable<NodeCredentialResponse> {
    return this.http.post<NodeCredentialResponse>(
      `${env.secrets_url}`, mapKeys(snakeCase, credentialData));
  }

  public updateNodeCredential(credential: NodeCredential): Observable<NodeCredential> {
    return this.http.patch<NodeCredential>(
      `${env.secrets_url}/id/${credential.id}`, credential);
  }

  public deleteNodeCredential(id: string): Observable<{}> {
    return this.http.delete(`${env.secrets_url}/id/${id}`);
  }
}
