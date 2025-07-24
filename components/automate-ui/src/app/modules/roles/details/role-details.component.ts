import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { identity, isNil } from 'lodash/fp';
import { Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { EntityStatus } from '../../../entities/entities';
import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { routeParams } from '../../../route.selectors';
import { GetRole } from '../../../entities/roles/role.actions';
import { roleFromRoute, getStatus } from '../../../entities/roles/role.selectors';
import { Role } from '../../../entities/roles/role.model';

@Component({
  standalone: false,
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
    this.layoutFacade.showSidebar(Sidebar.Settings);
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
    return JSON.stringify(role, null, '  ');
  }
}
