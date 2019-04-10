import { map, mergeMap } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';

import {
  ProfileActionTypes,
  ProfilesSearch,
  ProfilesSearchSuccess,
  ProfilesSearchSuccessPayload
} from './profile.actions';
import { ProfileRequests } from './profile.requests';

@Injectable()
export class ProfileEffects {
  constructor(
    private actions$: Actions,
    private requests: ProfileRequests
  ) {}

  @Effect()
  profilesSearch$ = this.actions$.pipe(
    ofType(ProfileActionTypes.PROFILES_SEARCH),
    mergeMap((action: ProfilesSearch) => this.requests.profilesSearch(action.payload)),
    map((resp: ProfilesSearchSuccessPayload) => new ProfilesSearchSuccess(resp)));
}
