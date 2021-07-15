import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { PolicyFile, CookbookLocks, IncludedPolicyLocks } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyFile } from 'app/entities/policy-files/policy-file.action';
import { policyFileFromRoute } from 'app/entities/policy-files/policy-file-details.selectors';

export type PolicyFileTabName = 'details' | 'runList' | 'attributes';

@Component({
  selector: 'app-policy-file-details',
  templateUrl: './policy-file-details.component.html',
  styleUrls: ['./policy-file-details.component.scss']
})

export class PolicyFileDetailsComponent implements OnInit, OnDestroy {
  public conflictError = false;
  public PolicyFile: PolicyFile;
  public tabValue: PolicyFileTabName = 'details';
  public serverId: string;
  public orgId: string;
  public name: string;
  public revision: string;
  public policyFileDetailsLoading = true;
  public url: string;

  private isDestroyed = new Subject<boolean>();
  public activeIncludedPolicies: string;
  public activeRunlist: string;
  public showRunList = false;
  public showIncludedPolicies = false;
  public cookbook_locks: CookbookLocks[] = [];
  public included_policy_locks: IncludedPolicyLocks[] = [];

  constructor(
    private router: Router,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      switch (fragment) {
        case 'details':
          this.tabValue = 'details';
          break;
        case 'runList':
          this.tabValue = 'runList';
          break;
        case 'attributes':
          this.tabValue = 'attributes';
          break;
      }
    });
    // load policy file details
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('revision'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name, revision_id]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.revision = revision_id;
      this.store.dispatch(new GetPolicyFile({
        server_id: server_id, org_id: org_id, name: name, revision: revision_id
      }));
    });
    this.store.select(policyFileFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(policyFile => {
      this.PolicyFile = policyFile;
      this.cookbook_locks = policyFile.cookbook_locks;
      this.included_policy_locks = policyFile.included_policy_locks;
      this.policyFileDetailsLoading = false;
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: PolicyFileTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  handlePolicyFileSelected() {
    if (!this.showIncludedPolicies) {
      this.showIncludedPolicies = true;
      this.activeIncludedPolicies = 'autoHeight';
    } else {
      this.showIncludedPolicies = false;
      this.activeIncludedPolicies = '';
    }
  }

  handleRunListSelected() {
    if (!this.showRunList) {
      this.showRunList = true;
      this.activeRunlist = 'autoHeight';

    } else {
      this.showRunList = false;
      this.activeRunlist = '';
    }
  }
}
