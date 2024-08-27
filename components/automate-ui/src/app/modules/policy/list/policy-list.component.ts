import { Component, OnInit, OnDestroy } from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core';
import { Store, select } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';

import { ChefSorters } from '../../../helpers/auth/sorter';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { loading } from '../../../entities/entities';
import { DeletePolicy, GetPolicies } from '../../../entities/policies/policy.actions';
import { allPolicies, getAllStatus } from '../../../entities/policies/policy.selectors';
import { Policy } from '../../../entities/policies/policy.model';
import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';

@Component({
  selector: 'app-policy-list',
  templateUrl: './policy-list.component.html',
  styleUrls: ['./policy-list.component.scss']
})
export class PolicyListComponent implements OnInit, OnDestroy {
  public sortedPolicies$: Observable<Policy[]>;
  public policyToDelete: Policy;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    store.pipe(
      select(getAllStatus),
      takeUntil(this.isDestroyed),
      map(loading)
    ).subscribe((isLoading) =>
      this.layoutFacade.ShowPageLoading(isLoading)
    );

    this.sortedPolicies$ = store.pipe(
      select(allPolicies),
      map(policies => ChefSorters.naturalSort(policies, 'name')));
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetPolicies());
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public status({ members }: Policy): string {
    return members.length > 0 ? 'In use' : 'No members';
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startPolicyDelete($event: MatOptionSelectionChange, policy: Policy): void {
    if ($event.isUserInput) {
      this.deleteModalVisible = true;
      this.policyToDelete = policy;
    }
  }

  public deletePolicy(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeletePolicy({id: this.policyToDelete.id}));
    this.telemetryService.track('Settings_Policies_Delete');
  }

  public inUseMessage(): string {
    return (this.policyToDelete && this.policyToDelete.members.length > 0) ?
      'and will disrupt access for all attached members' : '';
  }
}
