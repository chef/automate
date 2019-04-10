import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { NodeHistoryFilter, NodeHistoryCountsFilter } from '../../types/types';

@Injectable()
export class RunHistoryStore {
  private _nodeHistoryFilter: NodeHistoryFilter = {page: 0, pageSize: 10};
  private _nodeHistoryCountsFilter: NodeHistoryCountsFilter = {};
  private _filter: BehaviorSubject<NodeHistoryFilter>;
  private _countsFilter: BehaviorSubject<NodeHistoryCountsFilter>;

  constructor() {
    this._filter = new BehaviorSubject<NodeHistoryFilter>(this.nodeHistoryFilter);
    this._countsFilter =
      new BehaviorSubject<NodeHistoryCountsFilter>(this._nodeHistoryCountsFilter);
  }

  get nodeHistoryCountsFilter() {
    return this._nodeHistoryCountsFilter;
  }

  get nodeHistoryFilter() {
    return this._nodeHistoryFilter;
  }

  set nodeHistoryFilter(filter: NodeHistoryFilter) {
    this._nodeHistoryFilter = filter;
    this._filter.next(this._nodeHistoryFilter);
  }

  set nodeHistoryCountsFilter(countsFilter: NodeHistoryCountsFilter) {
    this._nodeHistoryCountsFilter = countsFilter;
    this._countsFilter.next(this._nodeHistoryCountsFilter);
  }

  get countsFilter() {
    return this._countsFilter;
  }

  get filter() {
    return this._filter;
  }

  addFilter(type, filter) {
    if (type !== 'page') {
      this._nodeHistoryFilter['page'] = 1;
    }

    if (type === 'startDate' || type === 'endDate') {
      this._nodeHistoryCountsFilter[type] = filter;
      this._countsFilter.next(this._nodeHistoryCountsFilter);
    }

    this._nodeHistoryFilter[type] = filter;
    this._filter.next(this._nodeHistoryFilter);
  }

  getFilter(type) {
    return this._nodeHistoryFilter[type];
  }

  removeFilter(type) {
    if (type === 'startDate' || type === 'endDate') {
      delete this._nodeHistoryCountsFilter[type];
      this._countsFilter.next(this._nodeHistoryCountsFilter);
    }

    delete this._nodeHistoryFilter[type];
    this._filter.next(this._nodeHistoryFilter);
  }
}
