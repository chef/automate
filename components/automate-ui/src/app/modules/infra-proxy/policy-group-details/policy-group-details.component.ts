import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { Router } from '@angular/router';
import { policyGoupFromRoute } from 'app/entities/policy-files/policy-group-details.selectors';
import { PolicyGroup } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroup } from 'app/entities/policy-files/policy-file.action';

export type PolicyGroupTabName = 'policyfiles';

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
  public show = false;
  public policyCount: number;
  public policies: Array<any>;
  public policyGroupDetailsLoading = true;
  public tabValue: PolicyGroupTabName = 'policyfiles';
  public url: string;
  private isDestroyed = new Subject<boolean>();

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
      switch (fragment) {
        case 'policyfiles':
          this.tabValue = 'policyfiles';
          break;
      }
    });
    // load policy Group details
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetPolicyGroup({
        server_id: server_id, org_id: org_id, name: name
      }));
    });

    this.store.select(policyGoupFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(policyGroup => {
      this.show = true;
      this.policyGroup = policyGroup;
      this.policyCount = policyGroup.policies.length;
      this.policies =  policyGroup.policies;
      this.policyGroupDetailsLoading = false;
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
}
