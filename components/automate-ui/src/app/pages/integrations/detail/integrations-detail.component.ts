import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { IntegrationsDetailState } from './integrations-detail.reducer';
import { integrationsDetail } from './integrations-detail.selectors';
import { includes, without } from 'lodash';
import { ManagerDeleteNodes } from 'app/entities/managers/manager.actions';

@Component({
  selector: 'app-integrations-detail',
  templateUrl: './integrations-detail.component.html',
  styleUrls: ['./integrations-detail.component.scss']
})
export class IntegrationsDetailComponent {

  managerDetail$: Observable<IntegrationsDetailState>;
  selectedNodes: string[] = [];

  constructor(
    private router: Router,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.managerDetail$ = this.store.select(integrationsDetail);
  }

  onPageChanged(page) {
    this.router.navigate([], { queryParams: { page }, queryParamsHandling: 'merge' });
  }

  deleteNodes() {
    this.store.dispatch(new ManagerDeleteNodes({ ids: this.selectedNodes }));
    this.selectedNodes = [];
  }

  selectNode(id: string) {
    this.selectedNodes.push(id);
  }

  unselectNode(id: string) {
    this.selectedNodes = without(this.selectedNodes, id);
  }

  selectAllNodes(event: Event, nodes: any[]) {
    if (event.currentTarget['checked']) {
      this.selectedNodes = [];
    } else {
      nodes.forEach((node) => {
        this.selectedNodes.push(node.id);
      });
    }
  }

  isNodeSelected(id: string) {
    return includes(this.selectedNodes, id);
  }

  isAutomateManager(type: string) {
    return type === 'automate';
  }

  hasSelectedNodes() {
    return this.selectedNodes.length > 0;
  }
}
