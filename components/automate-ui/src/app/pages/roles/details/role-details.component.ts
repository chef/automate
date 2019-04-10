import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { GetRole } from 'app/entities/roles/role.actions';
import { roleFromRoute } from 'app/entities/roles/role.selectors';
import { Role } from 'app/entities/roles/role.model';

@Component({
  selector: 'app-role-details',
  templateUrl: './role-details.component.html',
  styleUrls: ['./role-details.component.scss']
})

export class RoleDetailsComponent implements OnInit, OnDestroy {
  public role: Role;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(private store: Store<NgrxStateAtom>) {  }

  ngOnInit(): void {
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
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public roleToString(role: Role): string {
    return JSON.stringify(role);
  }
}
