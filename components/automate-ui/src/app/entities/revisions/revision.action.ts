import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Revision } from './revision.model';

export enum RevisionActionTypes {
  GET_ALL          = 'REVISIONS::GET_ALL',
  GET_ALL_SUCCESS  = 'REVISIONS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE  = 'REVISIONS::GET_ALL::FAILURE'
}

export interface RevisionsSuccessPayload {
  revisions: Revision[];
}

export class GetRevisions implements Action {
  readonly type = RevisionActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetRevisionsSuccess implements Action {
  readonly type = RevisionActionTypes.GET_ALL_SUCCESS;
  constructor(public payload) { }
}

export class GetRevisionsFailure implements Action {
  readonly type = RevisionActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RevisionActions =
  | GetRevisions
  | GetRevisionsSuccess
  | GetRevisionsFailure;
