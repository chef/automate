import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';
import { UserPreference } from './user-preferences.model';
import { userPreferencesTimezone } from './user-preferences.selector';
import { TestUpdateUserTimezone } from './user-preferences.actions';



@Injectable({ providedIn: 'root'})
export class UserPreferencesService {

  constructor(
    private store: Store<NgrxStateAtom>
  ) {}

  timezone$: Observable<UserPreference> = this.store.select(userPreferencesTimezone);

    // only for testing development - will be removed or modified before release
  testUpdateUserTimezone(timezone) {
    this.store.dispatch(new TestUpdateUserTimezone(timezone));
  }

}
