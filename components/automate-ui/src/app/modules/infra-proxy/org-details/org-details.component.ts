import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Params } from '@angular/router';
import { Observable, Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { NgrxStateAtom, RouterState } from '../../../ngrx.reducers';
import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { routeParams, previousRoute } from '../../../route.selectors';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';
import { FeatureFlagsService } from '../../../services/feature-flags/feature-flags.service';
import { EntityStatus } from '../../../entities/entities';
import { Org } from '../../../entities/orgs/org.model';
import { ActivatedRoute } from '@angular/router';
import {
  getStatus, orgFromRoute
} from '../../../entities/orgs/org.selectors';
import { GetOrg } from '../../../entities/orgs/org.actions';
import { ProjectConstants } from '../../../entities/projects/project.model';

const ORG_DETAILS_TAB_NAME = 'orgDetailsTab';

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
  public resetKeyTab = false;
  public rolesTab = false;
  public nodesTab = false;
  public dataBagsTab = false;
  public clientsTab = false;
  public policyFilesTab = false;
  public policyGroupsTab = false;
  public redirect = ' ';
  private isDestroyed = new Subject<boolean>();
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  public chefInfraViewsFeatureFlagOn: boolean;

  previousRoute$: Observable<RouterState>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private activatedRoute: ActivatedRoute,
    private telemetryService: TelemetryService,
    private featureFlagsService: FeatureFlagsService
  ) {
      // feature flag enable and disable the details tab
      this.chefInfraViewsFeatureFlagOn =
        this.featureFlagsService.getFeatureStatus('chefInfraTabsViews');

      this.previousRoute$ = this.store.select(previousRoute);
      this.activatedRoute.queryParams.subscribe(data =>
        this.redirect = data.redirect);
      // condition for breadcrumb to select specific tab
      this.previousRoute$.subscribe((params: Params) => {
        const path: string[] = params.url;
        if (path.includes('roles')) {
          this.resetTabs();
          this.rolesTab = true;
        } else if (path.includes('environments')) {
          this.resetTabs();
          this.environmentsTab = true;
        } else if (path.includes('policyfiles')) {
          this.resetTabs();
          this.policyFilesTab = true;
        } else if (path.includes('data-bags')) {
          this.resetTabs();
          this.dataBagsTab = true;
        } else if (path.includes('nodes')) {
          this.resetTabs();
          this.nodesTab = true;
        } else if (path.includes('cookbooks')) {
          this.resetTabs();
          this.cookbooksTab = true;
        } else if (path.includes('resetkey')) {
          this.resetTabs();
          this.resetKeyTab = true;
        } else if (path.includes('clients')) {
          this.resetTabs();
          this.clientsTab = true;
        } else if (path.includes('policyGroups')) {
          this.resetTabs();
          this.policyGroupsTab = true;
        }
      });
      if (this.redirect === 'cookbook') {
        this.resetTabs();
        this.cookbooksTab = true;
      }
    }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.store.dispatch(new GetOrg({ server_id: server_id, id: org_id }));
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(orgFromRoute as any)
    ]).pipe(
      filter(([getOrgSt, orgState]) => getOrgSt ===
        EntityStatus.loadingSuccess && !isNil(orgState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_getOrgSt, orgState]) => {
      this.org = { ...orgState };
    });
  }

  tabChange(tab: number) {
    // Tab indices here correspond with the order of `<app-tab>` elements in the template.
    switch (tab) {
      case 0:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'cookbooks');
        this.resetTabs();
        this.cookbooksTab = true;
        break;
      case 1:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'roles');
        this.resetTabs();
        this.rolesTab = true;
        break;
      case 2:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'environments');
        this.resetTabs();
        this.environmentsTab = true;
        break;
      case 3:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'dataBags');
        this.resetTabs();
        this.dataBagsTab = true;
        break;
      case 4:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'clients');
        this.resetTabs();
        this.clientsTab = true;
        break;
      case 5:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'nodes');
        this.resetTabs();
        this.nodesTab = true;
        break;
      case 6:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'policyfiles');
        this.resetTabs();
        this.policyFilesTab = true;
        break;
      case 7:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'policyGroups');
        this.resetTabs();
        this.policyGroupsTab = true;
        break;
      case 8:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'orgEdit');
        break;
      case 9:
        this.telemetryService.track(ORG_DETAILS_TAB_NAME, 'resetkey');
        break;
    }
  }

  resetAdminKeyRedirection(authFailure: boolean) {
    if (authFailure) {
      this.resetTabs();
      this.resetKeyTab = authFailure;
    }
  }

  private resetTabs() {
    this.cookbooksTab = false;
    this.environmentsTab = false;
    this.dataBagsTab = false;
    this.rolesTab = false;
    this.policyFilesTab = false;
    this.resetKeyTab = false;
    this.clientsTab = false;
    this.nodesTab = false;
    this.policyGroupsTab = false;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
