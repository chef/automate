import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
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
  public sortedRoles$: Observable<Role[]>;
  public isIAMv2$: Observable<boolean>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
    store.select(getAllStatus).pipe(map(loading)).subscribe((isloading) =>
      this.layoutFacade.ShowPageLoading(isloading));

    this.sortedRoles$ = store.select(allRoles).pipe(
      map((roles: Role[]) => ChefSorters.naturalSort(roles, 'name')));

    this.isIAMv2$ = store.select(isIAMv2);
  }

  ngOnInit(): void {
    this.layoutFacade.showSettingsSidebar();
    this.store.dispatch(new GetRoles());
  }
}
