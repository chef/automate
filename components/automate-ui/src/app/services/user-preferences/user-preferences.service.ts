import { Injectable } from '@angular/core';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store, select } from '@ngrx/store';
// import { Subscription } from 'rxjs';
// import { take } from 'rxjs/operators';
import { userPreferencesTimezone } from './user-preferences.selector';
// import { Observable } from 'rxjs';
// import { Subscription } from 'rxjs';


@Injectable({ providedIn: 'root'})
export class UserPreferencesService {


  constructor(
    private store: Store<NgrxStateAtom>
  ) {}

  getTimezone(): any {
    return this.store.select(userPreferencesTimezone);
  }

  getTimezone2(): any {
    return this.store.pipe(
      select(userPreferencesTimezone)
    ).subscribe(tz => tz.value);
  }
}
