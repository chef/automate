// Warning! There are a couple tslint suppressions in this file.
// The "no-unused-variable" ones should go away once we update TypeScript to 2.6 or greater.
// (see https://gist.github.com/tonysneed/bb6d442103a057578a9498f106e45ac5)
// Not sure why tslint complains about the use of "T" in the select method.

// This code comes verbatim (except for the tslint suppression) from
// https://blog.realworldfullstack.io/real-world-angular-part-9-unit-testing-c62ba20b1d93


import { Action } from '@ngrx/store';
import { Observable, BehaviorSubject } from 'rxjs';
import { map } from 'rxjs/operators';


export class MockStore<T> extends BehaviorSubject<T> {

  dispatch = (_action: Action): void => {
  }

  select = <_T, R>(pathOrMapFn: any, ..._paths: string[]): Observable<R> => {
    return map.call(this, pathOrMapFn);
  }

}
