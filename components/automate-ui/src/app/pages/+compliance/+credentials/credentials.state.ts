import { withLatestFrom, map, filter, switchMap } from 'rxjs/operators';
import { Action, Store } from '@ngrx/store';
import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { createSelector } from '@ngrx/store';
import { ROUTER_NAVIGATION, RouterNavigationAction } from '@ngrx/router-store';
import { assign } from 'lodash';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { environment } from '../../../../environments/environment';
import { NgrxStateAtom } from '../../../ngrx.reducers';

export const CREDENTIALS_STATE_SLICE_KEY_NAME = 'credentials';
export const GET_CREDENTIALS = 'GET_CREDENTIALS';
export const GET_CREDENTIALS_SUCCESS = 'GET_CREDENTIALS_SUCCESS';
export const DELETE_CREDENTIAL = 'DELETE_CREDENTIAL';
export const DELETE_CREDENTIAL_SUCCESS = 'DELETE_CREDENTIAL_SUCCESS';
const SECRETS_URL = environment.secrets_url;

interface KVData {
  key?: string;
  value?: string;
}

export enum CredentialTypes {
  None = <any>'none',
  SSH = <any>'ssh',
  WinRM = <any>'winrm',
  Sudo = <any>'sudo'
}

export interface Credential {
  id?: string;
  name: string;
  type: CredentialTypes;
  last_modified?: string;
  tags: Array<KVData>;
  data: Array<KVData>;
}

export interface CredentialsList {
  items: Credential[];
  total: number;
  page: number;
  per_page: number;
  sort: string;
  order: string;
}

export interface TouchCapableDirtyCapableFormControl {
  readonly invalid: boolean;
  readonly touched: boolean;
}

export interface CredentialsState {
  credentialsList: CredentialsList;
}

export const initialState: CredentialsState = {
  credentialsList: {
    items: [],
    total: 0,
    page: 1,
    per_page: 100,
    sort: 'name',
    order: null
  }
};

const credentialsStateSelector = state => state.credentials;
const credentialsListSelector = createSelector(credentialsStateSelector, state => {
  return state.credentialsList;
});
export const selectors = {
  credentialsState: credentialsStateSelector,
  credentialsList: credentialsListSelector
};

export function buildCredential(name: string, type: CredentialTypes,
                                username: string = ''): Credential {
  return {
    name,
    type,
    data: [
      {
        key: 'username',
        value: username
      }
    ],
    tags: []
  };
}

export interface CredentialsAction extends Action {
  payload: any;
}

export class CredentialsActions {
  public static getCredentials(payload): CredentialsAction {
    return {
      type: GET_CREDENTIALS,
      payload
    };
  }

  public static getCredentialsSuccess(payload): CredentialsAction {
    return {
      type: GET_CREDENTIALS_SUCCESS,
      payload
    };
  }

  public static deleteCredential(payload): CredentialsAction {
    return {
      type: DELETE_CREDENTIAL,
      payload
    };
  }

  public static deleteCredentialSuccess(payload): CredentialsAction {
    return {
      type: DELETE_CREDENTIAL_SUCCESS,
      payload
    };
  }
}

@Injectable()
export class CredentialsEffects {
  constructor(
    private httpClient: HttpClient,
    private actions$: Actions,
    private store: Store<NgrxStateAtom>) {
  }

  // to keep the state decoupled, we first process url state, then process persistent ( domain
  // model ) state.  this is a cleaner alternative to dispatching getCredentials() in OnInit
  // in the container smart component.
  @Effect()
  navigateToListCredentials$ = this.actions$.pipe(
    // this listens to actions relating to route changes in the store
    ofType(ROUTER_NAVIGATION),
    // this grabs the route path
    map((action: RouterNavigationAction) => action.payload.routerState),
    // this gets us just the route path change for the route we care about
    filter((routerState) => routerState['url'].startsWith('/settings/node-credentials')),
    map((routerState) => {
      const {page, per_page} = routerState['queryParams'];
      return assign({}, routerState['queryParams'], {
        filters: [
          {key: 'type', values: ['ssh', 'winrm', 'sudo']}
        ],
        page: page ? parseInt(page, 10) : 1,
        per_page: per_page ? parseInt(per_page, 10) : 100
      });
    }),
    map(CredentialsActions.getCredentials));

  @Effect()
  getCredentials$ = this.actions$.pipe(
    ofType(GET_CREDENTIALS),
    switchMap((action: CredentialsAction) => {
      const url = `${environment.secrets_url}/search`;
      const params = action.payload;
      if (params['order']) {
        params['order'] = params['order'].toUpperCase();
      }
      return this.httpClient.post(url, params);
    }),
    // If successful, this dispatches a success action with the result.  the container
    // smart component will listen for this in it's store.select, and pass the updated state
    // to it's child display components for rendering.
    map(CredentialsActions.getCredentialsSuccess));

  @Effect()
  deleteCred$ = this.actions$.pipe(
    ofType(DELETE_CREDENTIAL),
    switchMap((action: CredentialsAction) => {
      return this.httpClient.delete(`${SECRETS_URL}/id/${action.payload.id}`);
    }),
    map(CredentialsActions.deleteCredentialSuccess));

  @Effect()
  deleteCredSuccess$ = this.actions$.pipe(
    ofType(DELETE_CREDENTIAL_SUCCESS),
    withLatestFrom(this.store.select(selectors.credentialsList)),
    map(([_action, credentialsList]) => {
      const {page, per_page} = credentialsList;
      const filters =  [
        {key: 'type', values: ['ssh', 'winrm', 'sudo']}
      ];
      return {page, per_page, filters};
    }),
    map(CredentialsActions.getCredentials));
}

export function credentialsReducer(state: CredentialsState = initialState,
                                   action: CredentialsAction): CredentialsState {
  switch (action.type) {
    case GET_CREDENTIALS: {
      const credentialsList = assign({}, state.credentialsList, action.payload);
      return assign({}, state, {credentialsList});
    }

    case GET_CREDENTIALS_SUCCESS: {
      const credentialsList = assign({}, state.credentialsList, {
        items: action.payload.secrets || [],
        total: action.payload.total || 0
      });
      return assign({}, state, {credentialsList});
    }

    default: {
      return state;
    }
  }
}
