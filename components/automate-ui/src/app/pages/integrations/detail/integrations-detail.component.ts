import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from '../../../ngrx.reducers';
import { IntegrationsDetailState } from './integrations-detail.reducer';
import { integrationsDetail } from './integrations-detail.selectors';

@Component({
  selector: 'app-integrations-detail',
  templateUrl: './integrations-detail.component.html',
  styleUrls: ['./integrations-detail.component.scss']
})
export class IntegrationsDetailComponent {

  managerDetail$: Observable<IntegrationsDetailState>;

  constructor(
    private router: Router,
    private store: Store<NgrxStateAtom>
  ) {
    this.managerDetail$ = this.store.select(integrationsDetail);
  }

  onPageChanged(page) {
    this.router.navigate([], { queryParams: { page }, queryParamsHandling: 'merge' });
  }
}
