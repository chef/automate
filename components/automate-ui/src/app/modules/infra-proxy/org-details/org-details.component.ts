import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Params } from '@angular/router';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom, RouterState } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { previousRoute } from '../../../route.selectors';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';
import { EntityStatus } from 'app/entities/entities';
import { Org } from 'app/entities/orgs/org.model';
import {
  getStatus, orgFromRoute
} from 'app/entities/orgs/org.selectors';
import { GetOrg } from 'app/entities/orgs/org.actions';
import { ProjectConstants } from 'app/entities/projects/project.model';

@Component({
  selector: 'app-org-details',
  templateUrl: './org-details.component.html',
  styleUrls: ['./org-details.component.scss']
})

export class OrgDetailsComponent implements OnInit, OnDestroy {
  public org: Org;
  public loading$: Observable<boolean>;
  public serverId: string;
  public orgId: string;
  public cookbooksTab = true;
  public environmentsTab = false;
  public rolesTab = false;
  public dataBagsTab = false;
  public clientsTab = false;
  public policyFilesTab = false;
  private isDestroyed = new Subject<boolean>();
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  previousRoute$: Observable<RouterState>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
      this.previousRoute$ = this.store.select(previousRoute);

      // condition for breadcrumb to select specific tab
      this.previousRoute$.subscribe((params: Params) => {
        const path: string[] = params.path;

        if (path.includes('roles')) {
          this.resetTabs();
          this.rolesTab = true;
        } else if (path.includes('environments')) {
          this.resetTabs();
          this.environmentsTab = true;
        } else if (path.includes('policyFiles')) {
          this.resetTabs();
          this.policyFilesTab = true;
        } else if (path.includes('data_bags')) {
          this.resetTabs();
          this.dataBagsTab = true;
        } else if (path.includes('cookbooks')) {
          this.resetTabs();
          this.cookbooksTab = true;
        }
      });

    }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.store.dispatch(new GetOrg({ server_id: server_id, id: org_id }));
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(orgFromRoute)
    ]).pipe(
      filter(([getOrgSt, orgState]) => getOrgSt ===
        EntityStatus.loadingSuccess && !isNil(orgState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_getOrgSt, orgState]) => {
      this.org = { ...orgState };
    });
  }

  tabChange(tab: number) {
    switch (tab) {
      case 0:
        this.telemetryService.track('orgDetailsTab', 'cookbooks');
        break;
      case 1:
        this.telemetryService.track('orgDetailsTab', 'roles');
        break;
      case 2:
        this.telemetryService.track('orgDetailsTab', 'environments');
        break;
      case 3:
        this.telemetryService.track('orgDetailsTab', 'dataBags');
        break;
      case 4:
        this.telemetryService.track('orgDetailsTab', 'clients');
        break;
      case 5:
        this.telemetryService.track('orgDetailsTab', 'policyFiles');
        break;
      case 6:
        this.telemetryService.track('orgDetailsTab', 'orgEdit');
        break;
    }
  }

  private resetTabs() {
    this.cookbooksTab = false;
    this.environmentsTab = false;
    this.dataBagsTab = false;
    this.rolesTab = false;
    this.policyFilesTab = false;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
