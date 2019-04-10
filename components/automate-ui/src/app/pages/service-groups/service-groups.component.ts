import { map, takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable } from 'rxjs';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';
import { Chicklet, RollupServiceStatus } from '../../types/types';
import { EntityStatus } from '../../entities/entities';
import { UpdateServiceGroupFilters } from 'app/entities/service-groups/service-groups.actions';
import {
  ServiceGroup, ServiceGroupFilters
} from '../../entities/service-groups/service-groups.model';
import {
  serviceGroupStatus, allServiceGroups, serviceGroupState
} from '../../entities/service-groups/service-groups.selector';
import { find, includes } from 'lodash/fp';

@Component({
  selector: 'app-service-groups',
  templateUrl: './service-groups.component.html',
  styleUrls: ['./service-groups.component.scss']
})

export class ServiceGroupsComponent implements OnInit, OnDestroy {
  public serviceGroups$: Observable<ServiceGroup[]>;
  public serviceGroupStatus$: Observable<EntityStatus>;

  // The collection of allowable status
  private allowedStatus = ['ok', 'critical', 'warning', 'unknown'];

  // The currently selected health status filter
  selectedStatus$: Observable<string>;

  // Has this component been destroyed
  private isDestroyed: Subject<boolean> = new Subject();

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    const allUrlParameters$ = this.getAllUrlParameters();

    allUrlParameters$.pipe(takeUntil(this.isDestroyed)).subscribe(
      allUrlParameters => this.updateAllFilters(allUrlParameters));

    this.serviceGroupStatus$ = this.store.select(serviceGroupStatus);
    this.serviceGroups$ = this.store.select(allServiceGroups);

    this.selectedStatus$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.status));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public updateAllFilters(allParameters: Chicklet[]): void {
    const status = this.getSelectedStatus(allParameters);

    // Here we can add all the filters that the search bar will have
    const serviceGroupFilters: ServiceGroupFilters = {
      status: status
    };

    this.store.dispatch(new UpdateServiceGroupFilters({filters: serviceGroupFilters}));
  }

  public statusFilter(status) {
    const queryParams = {...this.route.snapshot.queryParams};
    if ( includes(status, this.allowedStatus) ) {
      queryParams['status'] = [status];
    } else {
      delete queryParams['status'];
    }

    delete queryParams['page'];

    this.router.navigate([], {queryParams});
  }

  private getSelectedStatus(allParameters: Chicklet[]): RollupServiceStatus {
    const status = find((chicklet) => {
        return chicklet.type === 'status';
      }, allParameters);

    if (status !== undefined && includes(status.text, this.allowedStatus)) {
      return status.text as RollupServiceStatus;
    } else {
      return undefined;
    }
  }

  private getAllUrlParameters(): Observable<Chicklet[]> {
    return this.route.queryParamMap.pipe(map((params: ParamMap) => {
      return params.keys.reduce((list, key) => {
        const paramValues = params.getAll(key);
        return list.concat(paramValues.map(value => ({type: key, text: value})));
      }, []);
    }));
  }
}
