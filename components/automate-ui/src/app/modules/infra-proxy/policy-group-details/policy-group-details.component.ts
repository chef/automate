import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { policyGoupFromRoute } from 'app/entities/policy-files/policy-group-details.selectors';
import { PolicyGroup } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroup } from 'app/entities/policy-files/policy-file.action';

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
  public policyGroupDetailsLoading = true;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
    ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

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
      this.policyGroupDetailsLoading = false;
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
