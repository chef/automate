import { Component, Input, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { EntityStatus, allLoaded } from 'app/entities/entities';
import { GetRoles } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import {
  allInfraRoles,
  getAllStatus as getAllRolesForOrgStatus
} from 'app/entities/infra-roles/infra-role.selectors';


@Component({
  selector: 'app-infra-roles',
  templateUrl: './infra-roles.component.html',
  styleUrls: ['./infra-roles.component.scss']
})

export class InfraRolesComponent implements OnInit {
  @Input() serverId: string;
  @Input() orgId: string;

  public roles: InfraRole[] = [];
  public loading$: Observable<boolean>;
  public isLoading = true;
  public rolesListLoading = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetRoles({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllRolesForOrgStatus)
    ]).pipe().subscribe(([getRolesSt]) => {
        this.isLoading = !allLoaded([getRolesSt]);
      });

    combineLatest([
      this.store.select(getAllRolesForOrgStatus),
      this.store.select(allInfraRoles)
    ]).pipe(
      filter(([getRolesSt, _allInfraRolesState]) =>
        getRolesSt === EntityStatus.loadingSuccess && !isNil(_allInfraRolesState))
    ).subscribe(([ _getRolesSt, allInfraRolesState]) => {
      this.roles = allInfraRolesState;
      this.rolesListLoading = false;
    });
  }
}
