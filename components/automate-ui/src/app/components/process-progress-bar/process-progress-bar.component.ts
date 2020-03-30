import { Component, OnInit, ViewEncapsulation, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { ApplyRulesStatus, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { ProjectService } from 'app/entities/projects/project.service';

@Component({
  selector: 'app-process-progress-bar',
  templateUrl: './process-progress-bar.component.html',
  styleUrls: ['./process-progress-bar.component.scss'],
  encapsulation: ViewEncapsulation.None
})
export class ProcessProgressBarComponent implements OnInit, OnDestroy {
  public confirmApplyStopModalVisible = false;
  public cancelRulesInProgress = false;
  public mode = 'determinate';
  public bufferValue = 0;
  public percentageComplete = 0;
  private isDestroyed = new Subject<boolean>();
  private applyRulesInProgress = false;

  constructor(
    public layoutFacade: LayoutFacadeService,
    public projects: ProjectService
    ) {}

  ngOnInit(): void {
    this.projects.applyRulesStatus$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(({ state, percentageComplete }: ApplyRulesStatus) => {
        if (this.applyRulesInProgress && state === ApplyRulesStatusState.NotRunning) {
          this.cancelRulesInProgress = false;
          this.percentageComplete = 0;
          this.bufferValue = this.percentageComplete - 100;
          this.closeConfirmApplyStopModal();
        }
        this.applyRulesInProgress = state === ApplyRulesStatusState.Running;
        if (!this.cancelRulesInProgress && state === ApplyRulesStatusState.Running) {
          this.percentageComplete = Math.floor(percentageComplete);
        }
        this.layoutFacade.layout.userNotifications.updatesProcessing = this.applyRulesInProgress;
        this.layoutFacade.updateDisplay();
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public openConfirmUpdateStopModal(): void {
    this.confirmApplyStopModalVisible = true;
  }

  private closeConfirmApplyStopModal(): void {
    this.confirmApplyStopModalVisible = false;
  }

  public confirmApplyStop(): void {
    this.cancelRulesInProgress = true;
    this.projects.applyRulesStop();
  }

  public cancelApplyStop(): void {
    this.closeConfirmApplyStopModal();
  }

  public displayPercent(): string {
    if (Math.floor(this.percentageComplete) === 0) {
      return 'Preparing...';
    } else if (Math.ceil(this.percentageComplete) === 100) {
      return 'Finishing Up...';
    }
    return `${this.percentageComplete.toString()}%`;
  }
}
