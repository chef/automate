import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import {
  clientRunsState
} from '../../entities/client-runs/client-runs.selectors';

@Component({
  selector: 'app-client-runs-sidebar',
  templateUrl: './client-runs-sidebar.component.html',
  styleUrls: ['./client-runs-sidebar.component.scss']
})
export class ClientRunsSidebarComponent implements OnInit {
  // Is workflow installed
  isWorkflowEnabled$: Observable<boolean>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.isWorkflowEnabled$ = this.store.select(createSelector(clientRunsState,
      (state) => state.workflowEnabled));
  }
}
