import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Client } from './client.model';

export enum ClientActionTypes {
  GET_ALL = 'CLIENTS::GET_ALL',
  GET_ALL_SUCCESS = 'CLIENTS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'CLIENTS::GET_ALL::FAILURE',
  GET = 'CLIENTS::GET',
  GET_SUCCESS = 'CLIENTS::GET::SUCCESS',
  GET_FAILURE = 'CLIENTS::GET::FAILURE',
  SEARCH = 'CLIENTS::SEARCH',
  SEARCH_SUCCESS = 'CLIENTS::SEARCH::SUCCESS',
  SEARCH_FAILURE = 'CLIENTS::SEARCH::FAILURE'
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

export interface ClientSearchPayload {
  clientName: string;
  server_id: string;
  org_id: string;
  page: number;
  per_page: number;
}

export class ClientSearch implements Action {
  readonly type = ClientActionTypes.SEARCH;
  constructor(public payload: ClientSearchPayload) { }
}

export interface ClientSearchSuccessPayload {
  clients: Client[];
}

export class ClientSearchSuccess implements Action {
  readonly type = ClientActionTypes.SEARCH_SUCCESS;
  constructor(public payload: ClientSearchSuccessPayload) { }
}

export class ClientSearchFailure implements Action {
  readonly type = ClientActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type ClientActions =
  | GetClients
  | GetClientsSuccess
  | GetClientsFailure
  | GetClient
  | GetClientSuccess
  | GetClientFailure
  | ClientSearch
  | ClientSearchSuccess
  | ClientSearchFailure;
