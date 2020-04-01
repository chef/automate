import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Router } from '@angular/router';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { EntityStatus, allLoaded, pending } from 'app/entities/entities';
import { Org } from 'app/entities/orgs/org.model';
import {
  getStatus, updateStatus, orgFromRoute
} from 'app/entities/orgs/org.selectors';
import { GetOrg, UpdateOrg } from 'app/entities/orgs/org.actions';
import { GetCookbooksForOrg } from 'app/entities/cookbooks/cookbook.actions';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import {
  allCookbooks,
  getAllStatus as getAllCookbooksForOrgStatus
} from 'app/entities/cookbooks/cookbook.selectors';
import { GetRolesForOrg } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import {
  allInfraRoles,
  getAllStatus as getAllRolesForOrgStatus
} from 'app/entities/infra-roles/infra-role.selectors';

export type OrgTabName = 'cookbooks' | 'roles' | 'details';

@Component({
  selector: 'app-org-details',
  templateUrl: './org-details.component.html',
  styleUrls: ['./org-details.component.scss']
})

export class OrgDetailsComponent implements OnInit, OnDestroy {
  public org: Org;
  public cookbooks: Cookbook[] = [];
  public roles: InfraRole[] = [];
  public loading$: Observable<boolean>;
  public sortedCookbooks$: Observable<Cookbook[]>;
  public saveSuccessful = false;
  public saveInProgress = false;
  public isLoading = true;
  public url: string;
  public serverId;
  public OrgId;
  public updateOrgForm: FormGroup;
  public tabValue: OrgTabName = 'cookbooks';
  private isDestroyed = new Subject<boolean>();

  cookbooksListLoading = true;
  rolesListLoading = true;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private router: Router
  ) {
    this.updateOrgForm = this.fb.group({
      name: new FormControl({value: ''}, [Validators.required]),
      admin_user: new FormControl({value: ''}, [Validators.required]),
      admin_key: new FormControl({value: ''}, [Validators.required])
    });
   }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      switch (fragment) {
        case 'details': {
          this.tabValue = 'details';
          break;
        }
        case 'cookbooks': {
          this.tabValue = 'cookbooks';
          break;
        }
        case 'roles': {
          this.tabValue = 'roles';
          break;
        }
      }
    });

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id]: string[]) => {
      this.serverId = server_id;
      this.OrgId = org_id;
      this.store.dispatch(new GetOrg({ server_id: server_id, id: org_id }));
      this.store.dispatch(new GetCookbooksForOrg({
        server_id: server_id, org_id: org_id
      }));
      this.store.dispatch(new GetRolesForOrg({
        server_id: server_id, org_id: org_id
      }));
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(updateStatus),
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(getAllRolesForOrgStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([getOrgSt, updateSt, getCookbooksSt, getRolesSt]) => {
        this.isLoading =
          !allLoaded([getOrgSt, getCookbooksSt, getRolesSt]) || updateSt === EntityStatus.loading;
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(orgFromRoute)
    ]).pipe(
      filter(([getOrgSt, _orgState]) =>
        getOrgSt === EntityStatus.loadingSuccess),
      filter(([_getOrgSt, orgState]) =>
        !isNil(orgState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_getOrgSt, orgState]) => {
      this.org = { ...orgState };
      this.updateOrgForm.controls['name'].setValue(this.org.name);
      this.updateOrgForm.controls['admin_user'].setValue(this.org.admin_user);
      this.updateOrgForm.controls['admin_key'].setValue(this.org.admin_key);
    });

    combineLatest([
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(allCookbooks)
    ]).pipe(
      filter(([getCookbooksSt, _allCookbooksState]) =>
        getCookbooksSt === EntityStatus.loadingSuccess),
      filter(([_getCookbooksSt, allCookbooksState]) =>
        !isNil(allCookbooksState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([ _getCookbooksSt, allCookbooksState]) => {
      this.cookbooks = allCookbooksState;
      this.cookbooksListLoading = false;
    });

    combineLatest([
      this.store.select(getAllRolesForOrgStatus),
      this.store.select(allInfraRoles)
    ]).pipe(
      filter(([getRolesSt, _allInfraRolesState]) =>
        getRolesSt === EntityStatus.loadingSuccess),
      filter(([_getRolesSt, allInfraRolesState]) =>
        !isNil(allInfraRolesState))
    ).subscribe(([ _getRolesSt, allInfraRolesState]) => {
      this.roles = allInfraRolesState;
      this.rolesListLoading = false;
    });

    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)))
      .subscribe((state) => {
        this.saveInProgress = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateOrgForm.markAsPristine();
        }
      });
  }

  onSelectedTab(event: { target: { value: OrgTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  saveOrg(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const name: string = this.updateOrgForm.controls.name.value.trim();
    const admin_user: string = this.updateOrgForm.controls.admin_user.value.trim();
    const admin_key: string = this.updateOrgForm.controls.admin_key.value.trim();
    this.store.dispatch(new UpdateOrg({
      org: {...this.org, name, admin_user, admin_key}
    }));
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

}
