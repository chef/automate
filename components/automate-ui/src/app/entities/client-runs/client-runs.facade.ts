import { Injectable } from '@angular/core';
import { Action, Store } from '@ngrx/store';

import { LayoutFacadeService } from '../layout/layout.facade';
import * as fromClientRuns from './client-runs.reducer';
import {
    clientRunsWorkflowEnabled,
    // clientRunsLoading,
    // clientRunsNodes,
    // clientRunsState,
    // clientRunsColumns
} from './client-runs.selectors';

@Injectable({
    providedIn: 'root'
})
export class ClientRunsFacadeService {
    private workflowEnabled: boolean;
    constructor(
        private store: Store<fromClientRuns.ClientRunsEntityState>,
        private layoutFacade: LayoutFacadeService
    ) {
        this.store.select(clientRunsWorkflowEnabled).subscribe(
            (workflowEnabled) => this.workflowEnabled = workflowEnabled
        );
    }

    dispatch(action: Action) {
        this.store.dispatch(action);
    }

    showSidebar() {
        this.layoutFacade.updateMenuGroups([{
            name: 'Infrastructure',
            items: [
                {
                    name: 'Client Runs',
                    icon: 'storage',
                    route: '/infrastructure/client-runs',
                    visible: true
                },
                {
                    name: 'Workflow',
                    icon: 'local_shipping',
                    route: '/workflow',
                    visible: this.workflowEnabled
                }
            ],
            visible: true
        }]);
    }
}
