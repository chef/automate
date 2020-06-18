import { Component, OnInit, OnDestroy } from '@angular/core';
import { timer as observableTimer, Subject } from 'rxjs';
import { takeUntil, withLatestFrom } from 'rxjs/operators';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { DateTime } from 'app/helpers/datetime/datetime';

import * as actions from '../../../entities/nodes/nodes.actions';
import * as selectors from '../../../entities/nodes/nodes.selectors';
import * as moment from 'moment/moment';
import { reject } from 'lodash';

@Component({
  selector: 'app-nodes-list',
  templateUrl: './nodes-list.component.html',
  styleUrls: ['./nodes-list.component.scss']
})

export class NodesListComponent implements OnInit, OnDestroy {
  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  public DateTime = DateTime;
  nodesList;
  nodesListSort = 'last_contact';
  nodesListSortOrder = 'DESC';
  nodesListFilters = [];
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  ngOnInit(): void {
    this.store.select(selectors.nodesList).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(nodesList => this.nodesList = nodesList);

    observableTimer(0, 100000).pipe(
      withLatestFrom(this.store.select(selectors.nodesList)),
      takeUntil(this.isDestroyed))
      .subscribe(([_i]) => {
        const params = {
          page: this.nodesList.page, per_page: 100,
          sort: this.nodesListSort, order: this.nodesListSortOrder,
          filters: this.nodesListFilters};
        this.store.dispatch(actions.getNodes(params));
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onPageChange(event): void {
    this.nodesList.page = event;
    const params = {
      page: this.nodesList.page, per_page: 100,
      sort: this.nodesListSort, order: this.nodesListSortOrder,
      filters: this.nodesListFilters};
    this.store.dispatch(actions.getNodes(params));
  }

  onSortToggled(event): void {
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
      return;
    }
    this.nodesListSort = sort;
    if (this.nodesListSortOrder === 'ASC') {
      this.nodesListSortOrder = 'DESC';
    } else {
      this.nodesListSortOrder = 'ASC';
    }
    const params = {
      page: this.nodesList.page, per_page: 100,
      sort: this.nodesListSort, order: this.nodesListSortOrder,
      filters: this.nodesListFilters};
    this.store.dispatch(actions.getNodes(params));
  }

  filterFor(type: string, item: string): void {
    if (type === 'last_contact') {
      // for a last contact filter, we send in two dates - beg of day and end of day
      this.nodesListFilters.push({key: type,
        values: [
          moment(item).startOf('day').toISOString(),
          moment(item).endOf('day').toISOString()
        ]
      });
    } else {
      this.nodesListFilters.push({key: type, values: [item]});
    }
    const params = {
      page: 1, per_page: 100,
      sort: this.nodesListSort, order: this.nodesListSortOrder,
      filters: this.nodesListFilters};
    this.store.dispatch(actions.getNodes(params));
  }

  displayCurrentFilters(): string[] {
    let filters = [];
    this.nodesListFilters.forEach((filter) => {
      filter.values.forEach((val) => {
        if (filter.exclude) {
        filters.push(filter.key + '::' + val + '::negated=true');
        } else {
          filters.push(filter.key + '::' + val + '::negated=false');
        }
      });
    });
    filters = filters.filter(function( element ) {
      return element !== undefined;
   });
    return filters;
  }

  isMatchingFilter(stringFilter: string): {filter: {key, values}} {
    const arr = stringFilter.split('::');
    for (let i = 0 ; i < this.nodesListFilters.length; i++) {
      if (this.nodesListFilters[i].key === arr[0]) {
        for (let j = 0 ; j < this.nodesListFilters[i].values.length; j++) {
          if (this.nodesListFilters[i].values[j] === arr[1]) {
            return {filter: this.nodesListFilters[i]};
          }
        }
      }
    }
    return undefined;
  }

  negateFilter(filter: string): void {
    const filterToNegate = this.isMatchingFilter(filter);
    if (filterToNegate !== undefined) {
      this.nodesListFilters = reject(
        this.nodesListFilters, function(item) { return item === filterToNegate.filter; });
      filterToNegate.filter['exclude'] = true;
      this.nodesListFilters.push(filterToNegate.filter);
      const params = {
        page: 1, per_page: 100,
        sort: this.nodesListSort, order: this.nodesListSortOrder,
        filters: this.nodesListFilters};
      this.store.dispatch(actions.getNodes(params));
    }
  }

  removeFilter(filter: string): void {
    const filterToRemove = this.isMatchingFilter(filter);
    if (filterToRemove !== undefined) {
      this.nodesListFilters = reject(
        this.nodesListFilters, function(item) { return item === filterToRemove.filter; });
      const params = {
        page: 1, per_page: 100,
        sort: this.nodesListSort, order: this.nodesListSortOrder,
        filters: this.nodesListFilters};
      this.store.dispatch(actions.getNodes(params));
    }
  }

  trackBy(node) {
    return node;
  }

  deleteNode(node) {
    this.store.dispatch(actions.deleteNode(node));
  }

  statusIcon(status: string): string {
    switch (status) {
      case ('FAILED'): return 'report_problem';
      case ('PASSED'): return 'check_circle';
      case ('UNKNOWN'): return 'help';
      case ('SKIPPED'): return 'help';
      default: return '';
    }
  }

  displayLastContact(time: string) {
    return moment(time, DateTime.REPORT_DATE_TIME);
  }

  displayNodeTags(tags: [{key, value}]): string {
    let stringTags = '';
    if (tags.length > 0) {
      tags.forEach((tag) => {
        stringTags = stringTags + tag.key + ':' + tag.value + ' ';
      });
    }
    return stringTags;
  }
}
