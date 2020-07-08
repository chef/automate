import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Client } from './client.model';

export enum ClientActionTypes {
  GET_ALL = 'CLIENTS::GET_ALL',
  GET_ALL_SUCCESS = 'CLIENTS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'CLIENTS::GET_ALL::FAILURE',
  GET = 'CLIENTS::GET',
  GET_SUCCESS = 'CLIENTS::GET::SUCCESS',
  GET_FAILURE = 'CLIENTS::GET::FAILURE'
}

export interface ClientsSuccessPayload {
  clients: Client[];
}

export class GetClients implements Action {
  readonly type = ClientActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string }) { }
}

export class GetClientsSuccess implements Action {
  readonly type = ClientActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: ClientsSuccessPayload) { }
}

export class GetClientsFailure implements Action {
  readonly type = ClientActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}


export class GetClient implements Action {
  readonly type = ClientActionTypes.GET;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetClientSuccess implements Action {
  readonly type = ClientActionTypes.GET_SUCCESS;
  constructor(public payload: Client) { }
}

export class GetClientFailure implements Action {
  readonly type = ClientActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type ClientActions =
  | GetClients
  | GetClientsSuccess
  | GetClientsFailure
  | GetClient
  | GetClientSuccess
  | GetClientFailure;
  