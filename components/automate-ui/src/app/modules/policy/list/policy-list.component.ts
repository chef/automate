import { Component, OnInit } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading } from 'app/entities/entities';
import { DeletePolicy, GetPolicies } from 'app/entities/policies/policy.actions';
import { allPolicies, getAllStatus, isIAMv2 } from 'app/entities/policies/policy.selectors';
import { Policy } from 'app/entities/policies/policy.model';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-policy-list',
  templateUrl: './policy-list.component.html',
  styleUrls: ['./policy-list.component.scss']
})
export class PolicyListComponent implements OnInit {
  public loading$: Observable<boolean>;
  public sortedPolicies$: Observable<Policy[]>;
  public isIAMv2$: Observable<boolean>;
  public policyToDelete: Policy;
  public deleteModalVisible = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
    this.loading$ = store.pipe(select(getAllStatus), map(loading));
    this.sortedPolicies$ = store.pipe(
      select(allPolicies),
      map(policies => ChefSorters.naturalSort(policies, 'name')));

    this.isIAMv2$ = store.pipe(select(isIAMv2));
  }

  ngOnInit(): void {
    this.layoutFacade.showSettingsSidebar();
    this.store.dispatch(new GetPolicies());
  }

  public status({ members }: Policy): string {
    return members.length > 0 ? 'In use' : 'No members';
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startPolicyDelete(policy: Policy): void {
    this.deleteModalVisible = true;
    this.policyToDelete = policy;
  }

  public deletePolicy(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeletePolicy({id: this.policyToDelete.id}));
  }

  public inUseMessage(): string {
    return (this.policyToDelete && this.policyToDelete.members.length > 0) ?
      'and will disrupt access for all attached members' : '';
  }
}
