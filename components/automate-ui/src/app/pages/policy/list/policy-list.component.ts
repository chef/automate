import { Component, OnInit } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading } from 'app/entities/entities';
import { DeletePolicy, GetPolicies } from 'app/entities/policies/policy.actions';
import { allPolicies, getAllStatus, iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { Policy } from 'app/entities/policies/policy.model';

@Component({
  selector: 'app-policy-list',
  templateUrl: './policy-list.component.html',
  styleUrls: ['./policy-list.component.scss']
})
export class PolicyListComponent implements OnInit {
  public loading$: Observable<boolean>;
  public sortedPolicies$: Observable<Policy[]>;
  public iamMajorVersion$: Observable<string>;
  public policyToDelete: Policy;
  public deleteModalVisible = false;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.loading$ = store.pipe(select(getAllStatus), map(loading));
    this.sortedPolicies$ = store.pipe(
      select(allPolicies),
      map(policies => policies.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // Sort by name then by cased-name, since no other field is useful as a secondary sort;
          // this ensures stable sort with respect to case, so 'a' always comes before 'A'.
          return a.name.localeCompare(b.name, undefined, opts)
            || a.name.localeCompare(b.name, undefined, {numeric: true});
        }
      )));

    this.iamMajorVersion$ = store.pipe(select(iamMajorVersion));
  }

  ngOnInit(): void {
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
