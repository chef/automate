import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading } from 'app/entities/entities';
import { GetRoles } from 'app/entities/roles/role.actions';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
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
  public isIAMv2$: Observable<boolean>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedRoles$ = store.select(allRoles).pipe(
      map((roles: Role[]) => ChefSorters.naturalSort(roles, 'name')));

    this.isIAMv2$ = store.select(isIAMv2);
  }

  ngOnInit(): void {
    this.store.dispatch(new GetRoles());
  }
}
