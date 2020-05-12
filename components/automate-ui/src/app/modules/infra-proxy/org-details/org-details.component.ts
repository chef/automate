import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
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
  public rolesTab = false;
  private isDestroyed = new Subject<boolean>();

  previousRoute$: Observable<RouterState>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
      this.previousRoute$ = this.store.select(previousRoute);

      // condition for breadcrumb to select specific tab
      this.previousRoute$.subscribe((params: any) => {
        if ( params.path.includes('roles') ) {
          this.cookbooksTab = false;
          this.rolesTab = true;
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
        this.telemetryService.track('orgDetailsTab', 'org-edit');
        break;
    }
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
