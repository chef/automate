import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Destination } from './destination.model';

export enum DestinationActionTypes {
  GET_ALL                        = 'DESTINATION::GET_ALL',
  GET_ALL_SUCCESS                = 'DESTINATION::GET_ALL::SUCCESS',
  GET_ALL_FAILURE                = 'DESTINATION::GET_ALL::FAILURE',
  GET                            = 'DESTINATION::GET',
  GET_SUCCESS                    = 'DESTINATION::GET::SUCCESS',
  GET_FAILURE                    = 'DESTINATION::GET::FAILURE',
  CREATE                         = 'DESTINATION::CREATE',
  CREATE_SUCCESS                 = 'DESTINATION::CREATE::SUCCESS',
  CREATE_FAILURE                 = 'DESTINATION::CREATE::FAILURE',
  UPDATE                         = 'DESTINATION::UPDATE',
  UPDATE_SUCCESS                 = 'DESTINATION::UPDATE::SUCCESS',
  UPDATE_FAILURE                 = 'DESTINATION::UPDATE::FAILURE',
  DELETE                         = 'DESTINATION::CREATE::DELETE',
  DELETE_SUCCESS                 = 'DESTINATION::CREATE::DELETE::SUCCESS',
  DELETE_FAILURE                 = 'DESTINATION::CREATE::DELETE::FAILURE'
}


export interface DestinationSuccessPayload {
  destination: Destination;
}

export interface GetDestinationsSuccessPayload {
  destinations: Destination[];
}

export class GetDestinations implements Action {
  readonly type = DestinationActionTypes.GET_ALL;
}

export class GetDestinationsSuccess implements Action {
  readonly type = DestinationActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: GetDestinationsSuccessPayload) { }
}

export class GetDestinationsFailure implements Action {
  readonly type = DestinationActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetDestination implements Action {
  readonly type = DestinationActionTypes.GET;

  constructor(public payload: { id: string }) { }
}

export class GetDestinationSuccess implements Action {
  readonly type = DestinationActionTypes.GET_SUCCESS;

  constructor(public payload: Destination) { }
}

export class GetDestinationFailure implements Action {
  readonly type = DestinationActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse, public id: string) { }
}

export interface CreateDesinationPayload {
  id?: string;
  name: string;
  url: string;
  secret?: string;
}

export class CreateDestination implements Action {
  readonly type = DestinationActionTypes.CREATE;
  constructor(public payload: CreateDesinationPayload, public username: string, public password: string ) { }
}

export class CreateDestinationSuccess implements Action {
  readonly type = DestinationActionTypes.CREATE_SUCCESS;
  constructor(public payload) { }
}

export class CreateDestinationFailure implements Action {
  readonly type = DestinationActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteDestination implements Action {
  readonly type = DestinationActionTypes.DELETE;
  constructor(public payload: { id: string, name: string }) { }
}

export class DeleteDestinationSuccess implements Action {
  readonly type = DestinationActionTypes.DELETE_SUCCESS;
  constructor(public payload: { id: string, name: string } ) { }
}

export class DeleteDestinationFailure implements Action {
  readonly type = DestinationActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateDestination implements Action {
  readonly type = DestinationActionTypes.UPDATE;

  constructor(public payload: { destination: Destination }) { }
}

export class UpdateDestinationSuccess implements Action {
  readonly type = DestinationActionTypes.UPDATE_SUCCESS;

  constructor(public payload) { }
}

export class UpdateDestinationFailure implements Action {
  readonly type = DestinationActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type DestinationActions =
  | GetDestinations
  | GetDestinationsSuccess
  | GetDestinationsFailure
  | GetDestination
  | GetDestinationSuccess
  | GetDestinationFailure
  | CreateDestination
  | CreateDestinationSuccess
  | CreateDestinationFailure
  | UpdateDestination
  | UpdateDestinationSuccess
  | UpdateDestinationFailure
  | DeleteDestination
  | DeleteDestinationSuccess
  | DeleteDestinationFailure;
