import { Component, Input, OnChanges, Output, EventEmitter } from '@angular/core';
import { ApplyRulesStatus } from 'app/entities/projects/project.reducer';
import * as moment from 'moment';

@Component({
  selector: 'app-confirm-apply-stop-modal',
  templateUrl: './confirm-apply-stop-modal.component.html',
  styleUrls: [ './confirm-apply-stop-modal.component.scss' ]
})
export class ConfirmApplyStopModalComponent implements OnChanges {

  @Input() visible = false;

  @Input() applyRulesStatus: ApplyRulesStatus;

  @Output() confirm: EventEmitter<void> = new EventEmitter();

  @Output() cancel: EventEmitter<void> = new EventEmitter();

  public progressValue: number;

  public progressPrefixText: string;

  public progressSuffixText: string;

  ngOnChanges() {
    this.updateProgress(this.applyRulesStatus);
  }

  public updateProgress(applyRulesStatus: ApplyRulesStatus): void {
    const { percentageComplete, estimatedTimeComplete } = applyRulesStatus;
    const now = moment();
    const etc = moment(estimatedTimeComplete);
    const dur = moment.duration(etc.diff(now));
    const pad = num => num.toString().padStart(2, '0');
    const durHours = pad(Math.floor(dur.asHours()));
    const durMins = pad(dur.minutes());
    const durSecs = pad(dur.seconds());
    const durCountdown = etc.diff(now) < 0 ? '00:00:00' : `${durHours}:${durMins}:${durSecs}`;

    this.progressValue = percentageComplete;
    this.progressPrefixText = `${Math.floor(percentageComplete * 100)}% complete`;
    this.progressSuffixText = `${durCountdown} until finished`;
  }
}
