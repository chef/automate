import { NgrxStateAtom, RouterState } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';
import { Component, OnInit, OnDestroy, ChangeDetectorRef } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Observable, Subscription } from 'rxjs';
import { NodeDetailsService } from '../../services/node-details/node-details.service';
import { NodeRun, RunInfo } from '../../types/types';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { previousRoute } from '../../route.selectors';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-node-details',
  templateUrl: './node-details.component.html',
  styleUrls: ['./node-details.component.scss']
})
export class NodeDetailsComponent implements OnInit, OnDestroy {
  nodeId: string;
  runId: string;
  endTime: Date;
  nodeRun: NodeRun = NodeRun.Null;
  previousRoute$: Observable<RouterState>;

  public modalIsVisible = false;
  public runHistoryVisible = false;
  private events: Subscription;

  constructor(
    private changeDetectorRef: ChangeDetectorRef,
    private router: Router,
    private route: ActivatedRoute,
    private eventService: NodeDetailsService,
    private telemetryService: TelemetryService,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
    this.previousRoute$ = this.store.select(previousRoute);
  }

  ngOnInit() {
    this.layoutFacade.showSidebar('infrastructure');
    // Scroll to the top of the view
    window.scrollTo(0, 0);
    this.nodeId = this.getRouteParam('node-id');
    this.route.data.subscribe((data: { nodeRun: NodeRun }) => {
      this.nodeRun = data.nodeRun;
      this.runId = this.getRouteParam('run-id');
      this.endTime = new Date(this.getRouteParam('end_time'));
    });

    this.events = this.eventService.showModal$.subscribe((show) => {
      this.toggleModal(show);
      this.changeDetectorRef.markForCheck();
    });
    this.changeDetectorRef.markForCheck();
  }

  ngOnDestroy() {
    this.events.unsubscribe();
  }

  tabChange(tab: number) {
    switch (tab) {
      case 0:
        this.telemetryService.track('nodeDetailsTab', 'resources');
        break;
      case 1:
        this.telemetryService.track('nodeDetailsTab', 'run_list');
        break;
      case 2:
        this.telemetryService.track('nodeDetailsTab', 'attributes');
        break;
    }
  }

  updateRunId(newRun: RunInfo): void {
    this.router.navigate(
      [
        'infrastructure/client-runs', this.nodeId, 'runs', newRun.runId, {end_time: newRun.endTime}
      ]
    );
  }

  toggleModal(status: boolean): void {
    this.modalIsVisible = status;
  }

  openRunHistory() {
    this.runHistoryVisible = true;
    document.getElementById('run-history-panel').focus();
  }

  closeRunHistory() {
    if (this.runHistoryVisible) {
      this.runHistoryVisible = false;
      document.getElementById('run-history-button').querySelector('button').focus();
    }
  }

  private getRouteParam(paramName: string): string | undefined {
    return this.route.snapshot.params[paramName];
  }
}
