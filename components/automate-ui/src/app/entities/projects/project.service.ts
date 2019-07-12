import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ApplyRulesStatus } from './project.reducer';
import * as selectors from './project.selectors';
import { ApplyRulesStart, ApplyRulesStop, GetApplyRulesStatus } from './project.actions';

@Injectable()
export class ProjectService {

  applyRulesStatus$: Observable<ApplyRulesStatus> = this.store.select(selectors.applyRulesStatus);

  constructor(private store: Store<NgrxStateAtom>) {}

  applyRulesStart() {
    this.store.dispatch(new ApplyRulesStart());
  }

  applyRulesStop() {
    this.store.dispatch(new ApplyRulesStop());
  }

  getApplyRulesStatus() {
    this.store.dispatch(new GetApplyRulesStatus());
  }
}
