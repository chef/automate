import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';
import { UserPreferenceTimeformat } from './user-preferences.model';
import { userPreferenceTimeformatSelector } from './user-preferences.selector';
import {
  GetUserPreferences,
  UpdateUserPreferences,
  SetUserTimeformatInternal
} from './user-preferences.actions';
import { SigninUiSetting } from './signin-ui-settings';

@Injectable({ providedIn: 'root'})
export class UserPreferencesService {
  public apiEndpoint: string;
  public uiSettings: SigninUiSetting;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {}

  timeformat$: Observable<UserPreferenceTimeformat> = this.store
  .select(userPreferenceTimeformatSelector);

  setUserTimeformatInternal(format: UserPreferenceTimeformat) {
    this.store.dispatch(new SetUserTimeformatInternal(format));
  }

  getUserPreferences() {
    this.store.dispatch(new GetUserPreferences());
  }

  updateUserPreferences(tz) {
    this.store.dispatch(new UpdateUserPreferences(tz));
  }

}
