import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil, take } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { Router } from '@angular/router';
import { policyGoupFromRoute } from 'app/entities/policy-files/policy-group-details.selectors';
import { PolicyGroup, IncludedPolicyLocks } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroup } from 'app/entities/policy-files/policy-file.action';
import { GetPolicyGroupNodes } from 'app/entities/infra-nodes/infra-nodes.actions';
import { getAllNodesStatus, policyGroupNodeList } from 'app/entities/infra-nodes/infra-nodes.selectors';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { EntityStatus } from 'app/entities/entities';
import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';

export type PolicyGroupTabName = 'policyfiles' | 'nodes';

@Component({
  selector: 'app-policy-group-details',
  templateUrl: './policy-group-details.component.html',
  styleUrls: ['./policy-group-details.component.scss']
})
export class PolicyGroupDetailsComponent implements OnInit, OnDestroy {
  public policyGroup: PolicyGroup;
  public serverId: string;
  public orgId: string;
  public name: string;
  public policyCount: number;
  public policies: IncludedPolicyLocks[];
  public policyGroupDetailsLoading = true;
  public isPolicyfileAvailable = false;
  public tabValue: PolicyGroupTabName = 'policyfiles';
  public url: string;
  public current_page = 1;
  public per_page = 9;
  public total: number;
  public nodes: {items: InfraNode[], total: number};
  public policyGroupNodesLoading = true;
  public isNodesAvailable = false;
  private isDestroyed = new Subject<boolean>();
  private timeFromNowPipe = new TimeFromNowPipe();

  constructor(
    private router: Router,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
    ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'nodes') ? 'nodes' : 'policyfiles';
    });

    // load policy Group details
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).pipe(take(1))
    .subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetPolicyGroup({
        server_id: server_id, org_id: org_id, name: name
      }));
      this.loadNodes();
    });

    this.store.select(policyGoupFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(policyGroup => {
      this.policyGroup = policyGroup;
      this.policyCount = policyGroup.policies.length;
      this.policies =  policyGroup.policies;
      this.policyGroupDetailsLoading = false;
      this.isPolicyfileAvailable = this.policyCount > 0 ? true : false;
    });

    combineLatest([
      this.store.select(getAllNodesStatus),
      this.store.select(policyGroupNodeList)
    ]).
    pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
      ).subscribe(([getAllSt, nodeList]) => {
        if (getAllSt === EntityStatus.loadingSuccess && !isNil(nodeList)) {
          this.nodes = nodeList;
          this.isNodesAvailable = (this.nodes.total > 0) ? true : false;
          this.total = this.nodes.total;
          this.policyGroupNodesLoading = false;
        } else if (getAllSt === EntityStatus.loadingFailure) {
          this.policyGroupNodesLoading = false;
          this.isNodesAvailable = false;
        }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: PolicyGroupTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  loadNodes() {
    this.store.dispatch(new GetPolicyGroupNodes({
      server_id: this.serverId,
      org_id: this.orgId,
      policyGroupName: this.name,
      page: this.current_page,
      per_page: this.per_page
    }));
  }

  timeFromNow(epochFormat: string) {
    const epchoTime = Number(epochFormat);
    const fromNowValue = this.timeFromNowPipe.transform(epchoTime);
    return fromNowValue === '-' ? '--' : fromNowValue;
  }

  onPageChange(event: number): void {
    this.current_page = event;
    this.loadNodes();
  }
}
