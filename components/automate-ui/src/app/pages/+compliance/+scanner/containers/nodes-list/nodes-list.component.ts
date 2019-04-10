import { timer as observableTimer,  Observable, Subject } from 'rxjs';

import { map, withLatestFrom, takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../../../../ngrx.reducers';
import * as selectors from '../../state/scanner.selectors';
import * as actions from '../../state/scanner.actions';

@Component({
  templateUrl: './nodes-list.component.html',
  styleUrls: ['./nodes-list.component.scss']
})
export class NodesListComponent implements OnInit, OnDestroy {
  constructor(
    private store: Store<NgrxStateAtom>,
    private route: ActivatedRoute,
    private router: Router
  ) {}

  nodesList;

  nodeDetail$: Observable<any>;

  nodeTotals$: Observable<any>;

  statusFilter$: Observable<string>;

  showNodeResults = false;

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  ngOnInit(): void {
    this.store.select(selectors.nodesList).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(nodesList => this.nodesList = nodesList);

    this.nodeDetail$ = this.store.select(selectors.nodeDetail);
    this.nodeTotals$ = this.store.select(selectors.nodeTotals);
    this.statusFilter$ = this.route.queryParams.pipe(map(params => params['status'] || 'all'));

    // poll for updated jobs data every 5 secs (arbitrary interval)
    observableTimer(0, 5000).pipe(
      withLatestFrom(this.store.select(selectors.nodesListParams)),
      takeUntil(this.isDestroyed))
      .subscribe(([_i, params]) => {
        this.store.dispatch(actions.getNodes(params));
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onPageChanged(page) {
    this.router.navigate([], {queryParams: {page}, queryParamsHandling: 'merge'});
  }

  onSortToggled(event) {
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }

    const queryParams = {...this.route.snapshot.queryParams, sort, order};
    this.router.navigate([], {queryParams});
  }

  onStatusFilterChanged(event) {
    let status = event.target.value;
    if (status === 'all') {
      status = undefined;
    }

    const queryParams = {...this.route.snapshot.queryParams, status, page: 1};
    this.router.navigate([], {queryParams});
  }

  onNodeResultsShow(_event, id) {
    this.showNodeResults = true;
    this.store.dispatch(actions.getNode(id));
  }

  onNodeResultsHide(_event) {
    this.showNodeResults = false;
  }

  trackBy(index, _item) {
    return index;
  }

  orderFor(sortKey) {
    const {sort, order} = this.nodesList;
    if (sortKey === sort) {
      return order;
    }
    return 'none';
  }

  deleteNode(node) {
    this.store.dispatch(actions.deleteNode(node));
  }

  rerunNode(node) {
    this.store.dispatch(actions.rerunNode(node));
  }
}
