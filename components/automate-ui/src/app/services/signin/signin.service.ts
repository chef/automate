import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Params } from '@angular/router';

const callbackPath = '/session/callback';
interface CallbackResponse {
    state: string;
    id_token: string;
}

@Injectable()
export class SigninService {

  constructor(private httpClient: HttpClient) {}

  callback(params: Params): Observable<CallbackResponse> {
    return this.httpClient.get<any>(callbackPath, {headers: {skip: 'true'}, params});
  }
}
