import { Component, OnInit } from '@angular/core';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

import {
  GetContentItems,
  InstallContentItem,
  DownloadContentItem
} from 'app/entities/cds/cds.actions';

import {
  contentItems
} from 'app/entities/cds/cds.selectors';

import {
  ContentItem
} from 'app/entities/cds/cds.model';

@Component({
  selector: 'app-desktop-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit {

  public contentItems$: Observable<ContentItem[]>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.store.dispatch(new GetContentItems());
    this.contentItems$ = this.store.select(contentItems);

    setTimeout(() => this.layoutFacade.hideSidebar());
  }

  installContentItem(itemId: string) {
    this.store.dispatch(new InstallContentItem({id: itemId}));
  }

  downloadContentItem(item: ContentItem) {
    this.store.dispatch(new DownloadContentItem( item ));
  }
}
