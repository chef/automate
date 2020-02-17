import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { EntityStatus, loading, allLoaded } from 'app/entities/entities';
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

export type OrgTabName = 'cookbooks' | 'details';
@Component({
  selector: 'app-cookbooks-list',
  templateUrl: './cookbooks-list.component.html',
  styleUrls: ['./cookbooks-list.component.scss']
})
export class CookbooksListComponent implements OnInit, OnDestroy {
  public org: Org;
  public cookbooks: Cookbook[] = [];
  public loading$: Observable<boolean>;
  public sortedCookbooks$: Observable<Cookbook[]>;
  private isDestroyed = new Subject<boolean>();
  public saveSuccessful = false;
  public saving = false;
  public isLoading = true;
  public url: string;
  public serverId;
  public OrgId;
  public updateOrgForm: FormGroup;
  public tabValue: OrgTabName = 'cookbooks';
  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private router: Router
  ) { }

  ngOnInit() {
    this.layoutFacade.showInfrastructureSidebar();

    this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'cookbooks';
    });

    this.updateOrgForm = this.fb.group({
      name: ['', [Validators.required]],
      admin_user: ['', [Validators.required]],
      admin_key: ['', [Validators.required]]
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
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(updateStatus),
      this.store.select(getAllCookbooksForOrgStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([getOrgSt, updateSt, getCookbooksSt]) => {
        this.isLoading =
          !allLoaded([getOrgSt, getCookbooksSt]) || updateSt === EntityStatus.loading;
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(orgFromRoute),
      this.store.select(allCookbooks)
    ]).pipe(
        filter(([getOrgSt, getCookbooksSt, _orgState, _allCookbooksState]) =>
          getOrgSt === EntityStatus.loadingSuccess &&
          getCookbooksSt === EntityStatus.loadingSuccess),
        filter(([_getOrgSt, _getCookbooksSt, orgState, allCookbooksState]) =>
          !isNil(orgState) && !isNil(allCookbooksState)),
        takeUntil(this.isDestroyed)
      ).subscribe(([_getOrgSt, _getCookbooksSt, orgState, allCookbooksState]) => {
        this.org = { ...orgState };
        this.cookbooks = allCookbooksState;
        this.updateOrgForm.controls['name'].setValue(this.org.name);
        this.updateOrgForm.controls['admin_user'].setValue(this.org.admin_user);
        this.updateOrgForm.controls['admin_key'].setValue(this.org.admin_key);
      });
  }

  onSelectedTab(event: { target: { value: OrgTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  saveOrg(): void {
    this.saveSuccessful = false;
    this.saving = true;
    this.store.dispatch(new UpdateOrg({
      org: this.org
    }));

    const pendingSave = new Subject<boolean>();
    this.store.select(updateStatus).pipe(
      filter(identity),
      takeUntil(pendingSave))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingSave.next(true);
          pendingSave.complete();
          this.saving = false;
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.updateOrgForm.markAsPristine();
          }
          this.updateOrgForm.controls['name'].setValue(this.org.name);
          this.updateOrgForm.controls['admin_user'].setValue(this.org.admin_user);
          this.updateOrgForm.controls['admin_key'].setValue(this.org.admin_key);
        }
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

}
