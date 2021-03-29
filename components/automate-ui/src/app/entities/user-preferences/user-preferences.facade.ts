import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable } from 'rxjs';

import { EntityStatus } from '../entities';
import * as fromServiceGroups from './user-preferences.reducer';

import {
  UserPreference
} from '../user-preferences/user-preferences.model';

import {
  userPreferencesList,
  userPreferencesError,
  userPreferencesStatus
} from './user-preferences.selector';

import {
  GetUserPreferences,
  UpdateUserPreferences
} from './user-preferences.actions';

@Injectable({
  providedIn: 'root'
})
export class UserPreferencesFacadeService {
  public userPreferences$: Observable<UserPreference[]>;
  public userPreferencesStatus$: Observable<EntityStatus>;
  public userPreferencesError$: Observable<HttpErrorResponse>;

  constructor(
      private store: Store<fromServiceGroups.UserPreferencesEntityState>
    ) {
    this.userPreferences$ = this.store.select(userPreferencesList);
    this.userPreferencesStatus$ = this.store.select(userPreferencesStatus);
    this.userPreferencesError$ = this.store.select(userPreferencesError);
  }

  getUserPreferences() {
    this.store.dispatch(new GetUserPreferences());
  }

  updateUserPreferences(userPreferences: UserPreference[]) {
    this.store.dispatch(new UpdateUserPreferences(userPreferences));
  }
}
