import { Router } from '@angular/router';
import { Component } from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';

import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { DateTime } from '../../../helpers/datetime/datetime';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { EntityStatus } from '../../../entities/entities';
import { Manager } from '../../../entities/managers/manager.model';
import { automateManager, cloudManagers, managerStatus } from '../../../entities/managers/manager.selectors';
import { DeleteManager } from '../../../entities/managers/manager.actions';
import { routeParams } from '../../../route.selectors';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';

@Component({
  selector: 'app-integrations',
  templateUrl: './integrations-list.component.html',
  styleUrls: ['./integrations-list.component.scss']
})
export class IntegrationsListComponent {
  public managers$: Observable<Manager[]>;
  public managerStatus$: Observable<EntityStatus>;
  public automateManager$: Observable<Manager>;
  public deletePromptVisible = false;
  public managerIdForDeletion = '';
  private sort: string;
  private order: string;

  public readonly RFC2822 = DateTime.RFC2822;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.managers$ = store.select(cloudManagers);
    this.managerStatus$ = store.select(managerStatus);
    this.automateManager$ = store.select(automateManager) as any;
    store.select(routeParams).subscribe((params) => {
      this.sort = params['sort'];
      this.order = params['order'];
    });
  }

  loading(status) {
    return status === 'loading';
  }

  resetModal(): void {
    this.managerIdForDeletion = '';
    this.deletePromptVisible = false;
  }

  beginDelete($event: MatOptionSelectionChange, id: string): void {
    if ($event.isUserInput) {
      this.managerIdForDeletion = id;
      this.deletePromptVisible = true;
    }
  }

  handleDelete(): void {
    this.store.dispatch(new DeleteManager({ id: this.managerIdForDeletion }));
    this.resetModal();
    this.telemetryService.track('Settings_NodeIntegrations_Delete');
  }

  handleEdit(id) {
    this.router.navigate(['/settings/node-integrations/edit', id]);
  }

  onSortToggled(event) {
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }

    const queryParams = {sort, order};

    this.router.navigate([], {queryParams} );
  }

  orderFor(sortKey) {
    if (sortKey === this.sort) {
      return this.order;
    }
    return 'none';
  }
}
