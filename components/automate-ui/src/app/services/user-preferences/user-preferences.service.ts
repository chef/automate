import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';
import { UserPreferenceTimeformat } from './user-preferences.model';
import { userPreferenceTimeformatSelector } from './user-preferences.selector';
import {
  GetUserPreferences,
  UpdateUserPreferences,
  TestUpdateUserTimeformat } from './user-preferences.actions';



@Injectable({ providedIn: 'root'})
export class UserPreferencesService {
  public apiEndpoint: string;
  constructor(
    private store: Store<NgrxStateAtom>
  ) {}

  timeformat$: Observable<UserPreferenceTimeformat> = this.store.select(userPreferenceTimeformatSelector);

    // only for testing development - will be removed or modified before release
  testUpdateUserTimeformat(format: string) {
    this.store.dispatch(new TestUpdateUserTimeformat(format));
  }

  getUserPreferences() {
    this.store.dispatch(new GetUserPreferences());
  }

  updateUserPreferences(tz) {
    this.store.dispatch(new UpdateUserPreferences(tz));
  }

}
