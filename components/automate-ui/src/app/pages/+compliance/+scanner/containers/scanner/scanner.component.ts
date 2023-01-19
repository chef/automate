import { map } from 'rxjs/operators';
import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import * as selectors from '../../state/scanner.selectors';
import { LoadRemoteModuleOptions } from '@angular-architects/module-federation';
import { LookupService } from '../../../mfe-proxy/plugin/lookup.service';

type PluginOptions = LoadRemoteModuleOptions & {
  displayName: string;
  componentName: string;
}

@Component({
  templateUrl: './scanner.component.html',
  styleUrls: ['./scanner.component.scss']
})

export class ScannerComponent implements OnInit {

  jobsCount$: Observable<number>;
  nodesCount$: Observable<number>;
  jobsCountLoaded = false;
  nodesCountLoaded = false;

  plugins: PluginOptions[] = [];
  workflow: PluginOptions[] = [];

  constructor(
    private store: Store<any>,
    private layoutFacade: LayoutFacadeService,
    public lookupService: LookupService
  ) { }

  async ngOnInit(): Promise<void> {
    this.plugins = await this.lookupService.lookup();
    this.layoutFacade.ShowPageLoading(true);
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.jobsCount$ = this.store.select(selectors.jobsList)
      .pipe(map(jobsList => jobsList.total));
    this.jobsCountLoaded = true;

    this.nodesCount$ = this.store.select(selectors.nodeTotals)
      .pipe(map(nodeTotals => nodeTotals.all));
    this.nodesCountLoaded = true;
    this.layoutFacade.ShowPageLoading(false);
  }

  add(plugin: PluginOptions): void {
    console.warn('@@@@@####', plugin)
    this.workflow.push(plugin);
  }

}
