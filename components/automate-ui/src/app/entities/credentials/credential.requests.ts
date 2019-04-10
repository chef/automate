import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { environment as env } from '../../../environments/environment';
import { Credential } from './credential.model';

export interface CredentialSearchResponse {
  secrets: Credential[];
}

@Injectable()
export class CredentialRequests {

  constructor(private http: HttpClient) {}

  public search(params) {
    return this.http.post<CredentialSearchResponse>(`${env.secrets_url}/search`, params);
  }
}
