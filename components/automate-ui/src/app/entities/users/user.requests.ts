import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { mapKeys, snakeCase } from 'lodash/fp';

import { environment as env } from 'environments/environment';
import { CreateUserPayload } from './user.actions';
import { User } from './user.model';
import { SelfUser } from './userself.model';

export interface UsersResponse {
  users: User[];
}

@Injectable()
export class UserRequests {

  constructor(private http: HttpClient) { }

  public getUsers(): Observable<UsersResponse> {
    return this.http.get<UsersResponse>(`${env.auth_url}/users`);
  }

  public getUserByUsername(username: string): Observable<User> {
    return this.http.get<User>(`${env.auth_url}/users/${username}`);
  }

  public createUser(userData: CreateUserPayload): Observable<User> {
    return this.http.post<User>(`${env.auth_url}/users`, mapKeys(snakeCase, userData));
  }

  public updateUser(userData: User): Observable<User> {
    return this.http.put<User>(`${env.auth_url}/users/${userData.username}`,
      mapKeys(snakeCase, userData));
  }

  public updateSelf(userData: SelfUser): Observable<User> {
    return this.http.put<User>(`${env.users_url}/${userData.username}`,
      mapKeys(snakeCase, userData));
  }

  public deleteUser(user: User): Observable<User> {
    return this.http.delete<User>(`${env.auth_url}/users/${user.username}`).pipe(
      map(() => user)); // bounce it back, so it can be used
  }
}
