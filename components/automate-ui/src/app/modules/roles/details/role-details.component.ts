import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { identity, isNil } from 'lodash/fp';
import { Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { GetRole } from 'app/entities/roles/role.actions';
import { roleFromRoute, getStatus } from 'app/entities/roles/role.selectors';
import { Role } from 'app/entities/roles/role.model';

@Component({
  selector: 'app-role-details',
  templateUrl: './role-details.component.html',
  styleUrls: ['./role-details.component.scss']
})

export class RoleDetailsComponent implements OnInit, OnDestroy {
  public role: Role;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar('settings');
    this.store.select(roleFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((state) => {
        this.role = <Role>Object.assign({}, state);
      });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetRole({ id }));
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(roleFromRoute)
    ]).pipe(
      filter(([status, _]) => status === EntityStatus.loadingSuccess),
      filter(([_, roleState]) => !isNil(roleState)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, roleState]) => {
        this.role = <Role>Object.assign({}, roleState);
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public roleToString(role: Role): string {
    return JSON.stringify(role);
  }
}
