import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import { ApiToken, ApiTokenV1, isApiTokenV1, ensureApiTokenV2 } from './api-token.model';
import { CreateTokenPayload, GetAllTokensSuccessPayload } from './api-token.actions';

export interface TokenPayloadResponse {
  token: ApiToken;
}

export interface GetAllTokensResponse<T> {
  tokens: T[];
}

// V1 would return the token payload as sole response body; in v2, you get
//    { token: { TOKEN PAYLOAD } }
// this method lets you differentiate:
function isGetTokenResponseV1 (resp: ApiTokenV1 | TokenPayloadResponse):
  resp is TokenPayloadResponse {
  return (<TokenPayloadResponse>resp).token !== undefined;
}

function isGetAllTokensResponseV1(
  resp: GetAllTokensResponse<ApiTokenV1> | GetAllTokensResponse<ApiToken>):
  resp is GetAllTokensResponse<ApiTokenV1> {
  return (resp.tokens.length === 0) || isApiTokenV1(resp.tokens[0]);
}

export function versionizeToken(resp: ApiTokenV1 | TokenPayloadResponse): ApiToken {
  return isGetTokenResponseV1(resp) ? resp.token : ensureApiTokenV2(resp);
}

export function versionizeTokens(
  resp: GetAllTokensResponse<ApiTokenV1> | GetAllTokensResponse<ApiToken>)
: GetAllTokensSuccessPayload {
    return isGetAllTokensResponseV1(resp) ? { tokens: resp.tokens.map(ensureApiTokenV2) } : resp;
}

@Injectable()
export class ApiTokenRequests {

  constructor(private http: HttpClient) {}

  public getAll(version: IAMMajorVersion = 'v1'):
    Observable<GetAllTokensResponse<ApiToken> | GetAllTokensResponse<ApiTokenV1>> {
    switch (version) {
      case 'v2': {
        return this.http.get<GetAllTokensResponse<ApiToken>>(`${env.auth_v2_url}/tokens`);
      }
      default: {
        return this.http.get<GetAllTokensResponse<ApiTokenV1>>(`${env.auth_url}/tokens`);
      }
    }
  }

  public get(id: string, version: IAMMajorVersion = 'v1'):
    Observable<ApiTokenV1 | TokenPayloadResponse> {
    switch (version) {
      case 'v2': {
        return this.http.get<TokenPayloadResponse>(`${env.auth_v2_url}/tokens/${id}`);
      }
      default: {
        return this.http.get<ApiTokenV1>(`${env.auth_url}/tokens/${id}`);
      }
    }
  }

  public update(token: ApiToken, version: IAMMajorVersion = 'v1'):
    Observable<ApiTokenV1 | TokenPayloadResponse> {
    switch (version) {
      case 'v2': {
        return this.http.put<TokenPayloadResponse>(`${env.auth_v2_url}/tokens/${token.id}`, token);
      }
      default: {
        return this.http.put<ApiTokenV1>(`${env.auth_url}/tokens/${token.id}`,
         { ...token, description: token.name });
      }
    }
  }

  public create(tokenData: CreateTokenPayload, version: IAMMajorVersion = 'v1'):
    Observable<ApiTokenV1 | TokenPayloadResponse> {
    switch (version) {
      case 'v2': {
        return this.http.post<TokenPayloadResponse>(`${env.auth_v2_url}/tokens`,
          { 
            id: tokenData.id,
            name: tokenData.name,
            active: true,
            projects: tokenData.projects
          });
      }
      default: {
        return this.http.post<ApiTokenV1>(`${env.auth_url}/tokens`,
          { id: tokenData.id, description: tokenData.name, active: true });
      }
    }
  }

  // Note: toggleActive takes the EXISTING value in `active`, and will take care
  // of flipping it.
  public toggleActive(id: string, active: boolean, version: IAMMajorVersion = 'v1'):
    Observable<ApiTokenV1 | TokenPayloadResponse> {
    switch (version) {
      case 'v2': {
        return this.http.put<TokenPayloadResponse>(`${env.auth_v2_url}/tokens/${id}`,
          { active: !active });
      }
      default: {
        return this.http.put<ApiTokenV1>(`${env.auth_url}/tokens/${id}`,
          { active: !active });
      }
    }
  }

  public delete(id: string, version: IAMMajorVersion = 'v1'): Observable<Object> {
    switch (version) {
      case 'v2': {
        return this.http.delete(`${env.auth_v2_url}/tokens/${id}`);
      }
      default: {
        return this.http.delete(`${env.auth_url}/tokens/${id}`);
      }
    }
  }
}
