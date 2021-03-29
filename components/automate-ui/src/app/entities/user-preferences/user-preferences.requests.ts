import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';

import {
  UserPreference,
  UserPreferencesPayload
} from './user-preferences.model';
import { environment } from 'environments/environment';
const APPLICATIONS_URL = environment.applications_url;

@Injectable()
export class UserPreferencesRequests {

  constructor(private httpClient: HttpClient) {}

  public fetchUserPreferences(): Observable<UserPreferencesPayload> {
    console.log('fetching user preferences');
    const url = `${APPLICATIONS_URL}/user-preferences`;
    return this.httpClient.get<UserPreferencesPayload>(url);
  }

  public updateUserPreferences(userPreferences: UserPreference[]): Observable<any> {
    const url = `${APPLICATIONS_URL}/user-preferences`;
    return this.httpClient.post<UserPreference[]>(url, userPreferences);
  }
}
