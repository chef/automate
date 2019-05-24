import { map } from 'rxjs/operators';
import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import * as selectors from '../../state/scanner.selectors';

@Component({
  templateUrl: './scanner.component.html',
  styleUrls: ['./scanner.component.scss']
})
export class ScannerComponent implements OnInit {

  jobsCount$: Observable<number>;
  nodesCount$: Observable<number>;

  // show spinner before loading data
  countsLoading = true;
  jobsCountLoaded = false;
  nodesCountLoaded = false;

  constructor(private store: Store<any>) {}

  ngOnInit() {
    this.jobsCount$ = this.store.select(selectors.jobsList)
      .pipe(map(jobsList => jobsList.total));
    this.jobsCountLoaded = true;

    this.nodesCount$ = this.store.select(selectors.nodeTotals)
      .pipe(map(nodeTotals => nodeTotals.all));
    this.nodesCountLoaded = true;

    this.countsLoading = false;
  }
}
