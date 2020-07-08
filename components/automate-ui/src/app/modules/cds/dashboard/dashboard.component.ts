import { Component, OnInit, OnDestroy } from '@angular/core';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Observable, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { Store } from '@ngrx/store';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';

import {
  IsContentEnabled,
  GetContentItems,
  InstallContentItem,
  DownloadContentItem,
  SubmitCredentials
} from 'app/entities/cds/cds.actions';

import {
  contentItems,
  contentEnabled
} from 'app/entities/cds/cds.selectors';

import {
  ContentItem,
  Credentials
} from 'app/entities/cds/cds.model';

@Component({
  selector: 'app-desktop-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit, OnDestroy {

  public contentItems$: Observable<ContentItem[]>;
  public isContentEnabled$: Observable<boolean>;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private chefSessionService: ChefSessionService
  ) { }

  ngOnInit() {
    this.store.dispatch(new IsContentEnabled());

    this.contentItems$ = this.store.select(contentItems);
    this.isContentEnabled$ = this.store.select(contentEnabled);

    this.isContentEnabled$.pipe(
      takeUntil(this.isDestroyed),
      filter(isEnabled => isEnabled))
      .subscribe(_ => {
        this.store.dispatch(new GetContentItems());
      });

    setTimeout(() => this.layoutFacade.hideSidebar());
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  installContentItem(itemId: string) {
    this.store.dispatch(new InstallContentItem({
      id: itemId,
      user: this.chefSessionService.username
    }));
  }

  downloadContentItem(item: ContentItem) {
    this.store.dispatch(new DownloadContentItem( item ));
  }

  credentialsSubmit(credentials: Credentials) {
    this.store.dispatch(new SubmitCredentials( {credentials} ));
  }
}
