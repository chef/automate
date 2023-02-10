import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { ApiToken } from './api-token.model';
import { CreateTokenPayload } from './api-token.actions';

export interface TokenPayloadResponse {
  token: ApiToken;
}

export interface GetAllTokensResponse<T> {
  tokens: T[];
}

@Injectable()
export class ApiTokenRequests {

  constructor(private http: HttpClient) {}

  public getAll(): Observable<GetAllTokensResponse<ApiToken>> {
    return this.http.get<GetAllTokensResponse<ApiToken>>(`${env.iam_url}/tokens`);
  }

  public get(id: string): Observable<TokenPayloadResponse> {
    return this.http.get<TokenPayloadResponse>(`${env.iam_url}/tokens/${id}`);
  }

  public update(token: ApiToken): Observable<TokenPayloadResponse> {
    return this.http.put<TokenPayloadResponse>(`${env.iam_url}/tokens/${token.id}`, token);
  }

  public create({ id, name, projects }: CreateTokenPayload): Observable<TokenPayloadResponse> {
    return this.http.post<TokenPayloadResponse>(`${env.iam_url}/tokens`,
      { id, name, projects, active: true }
    );
  }

  // Note: toggleActive takes the EXISTING value in `active`, and will take care
  // of flipping it.
  public toggleActive(id: string, name: string, active: boolean, projects: string[]): Observable<TokenPayloadResponse> {
    return this.http.put<TokenPayloadResponse>(`${env.iam_url}/tokens/${id}`,
      { name, active: !active,projects: projects});
  }

  public delete(id: string): Observable<Object> {
    return this.http.delete(`${env.iam_url}/tokens/${id}`);
  }
}
