import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { Credential,
         CredentialsList,
         CredentialsActions,
         selectors } from '../credentials.state';
import { NgrxStateAtom } from '../../../../ngrx.reducers';

@Component({
  selector: 'app-credentials-list-screen',
  template: `
    <div class="content-container">
      <app-credentials-list
        class="container"
        [credentialsList]="credentialsList$ | async"
        (deleteCredential)="handleDeleteCredential($event)"
        (pageChanged)="handlePageChanged($event)"
        (sortToggled)="handleSortToggled($event)"
      ></app-credentials-list>
    </div>
  `
})

export class CredentialsListScreenComponent {
  credentialsList$: Observable<CredentialsList>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private router: Router,
    private route: ActivatedRoute
  ) {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.credentialsList$ = store.select(selectors.credentialsList);
  }

  handleDeleteCredential(credential: Credential) {
    this.store.dispatch(CredentialsActions.deleteCredential(credential));
  }

  handlePageChanged(page: number) {
    this.router.navigate([], {queryParams: {page}, queryParamsHandling: 'merge'});
  }

  handleSortToggled({sort, order}) {
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }

    const {page, per_page} = this.route.snapshot.queryParams;
    const queryParams = {sort, order, page, per_page};

    this.router.navigate([], {queryParams});
  }
}
