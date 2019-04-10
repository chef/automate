import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading } from 'app/entities/entities';
import { GetRoles } from 'app/entities/roles/role.actions';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { allRoles, getAllStatus } from 'app/entities/roles/role.selectors';
import { Role } from 'app/entities/roles/role.model';

@Component({
  selector: 'app-roles-list',
  templateUrl: './roles-list.component.html',
  styleUrls: ['./roles-list.component.scss']
})
export class RolesListComponent implements OnInit {
  public loading$: Observable<boolean>;
  public sortedRoles$: Observable<Role[]>;
  public iamMajorVersion$: Observable<string>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedRoles$ = store.select(allRoles).pipe(
      map((roles: Role[]) => roles.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // Sort by name then by cased-name, since no other field is useful as a secondary sort;
          // this ensures stable sort with respect to case, so 'a' always comes before 'A'.
          return a.name.localeCompare(b.name, undefined, opts)
            || a.name.localeCompare(b.name, undefined, {numeric: true});
        }
      )));

    this.iamMajorVersion$ = store.select(iamMajorVersion);
  }

  ngOnInit(): void {
    this.store.dispatch(new GetRoles());
  }
}
