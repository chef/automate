import { map } from 'rxjs/operators';
import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import * as selectors from '../../state/scanner.selectors';

@Component({
  templateUrl: './scanner.component.html',
  styleUrls: ['./scanner.component.scss']
})
export class ScannerComponent implements OnInit {

  jobsCount$: Observable<number>;
  nodesCount$: Observable<number>;
  jobsCountLoaded = false;
  nodesCountLoaded = false;

  constructor(
    private store: Store<any>,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.ShowPageLoading(true);
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.jobsCount$ = this.store.select(selectors.jobsList)
      .pipe(map(jobsList => jobsList.total));
    this.jobsCountLoaded = true;

    this.nodesCount$ = this.store.select(selectors.nodeTotals)
      .pipe(map(nodeTotals => nodeTotals.all));
    this.nodesCountLoaded = true;
    this.layoutFacade.ShowPageLoading(false);
  }
}
