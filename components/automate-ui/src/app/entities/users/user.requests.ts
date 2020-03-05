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

interface UserResponse {
  user: User;
}

@Injectable()
export class UserRequests {

  constructor(private http: HttpClient) { }

  public getUsers(): Observable<UsersResponse> {
    return this.http.get<UsersResponse>(`${env.iam_url}/users`);
  }

  public getUser(id: string): Observable<User> {
    return this.http.get<UserResponse>(`${env.iam_url}/users/${id}`)
      .pipe(map(userResponse => userResponse.user));
  }

  public createUser(userData: CreateUserPayload): Observable<User> {
    return this.http.post<UserResponse>(`${env.iam_url}/users`, mapKeys(snakeCase, userData))
      .pipe(map(userResponse => userResponse.user));
  }

  public updateUser(userData: User): Observable<User> {
    return this.http.put<UserResponse>(`${env.iam_url}/users/${userData.id}`,
      mapKeys(snakeCase, userData))
        .pipe(map(userResponse => userResponse.user));
  }

  public updateSelf(userData: SelfUser): Observable<User> {
    return this.http.put<UserResponse>(`${env.iam_url}/self/${userData.id}`,
      mapKeys(snakeCase, userData))
        .pipe(map(userResponse => userResponse.user));
  }

  public deleteUser(user: User): Observable<User> {
    return this.http.delete<UserResponse>(`${env.iam_url}/users/${user.id}`).pipe(
      map(() => user)); // bounce it back, so it can be used
  }
}
