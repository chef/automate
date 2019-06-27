import { Component, Input, OnChanges, Output, EventEmitter } from '@angular/core';
import { ApplyRulesStatus } from 'app/entities/projects/project.reducer';
import * as moment from 'moment';

@Component({
  selector: 'app-confirm-apply-stop-modal',
  templateUrl: './confirm-apply-stop-modal.component.html',
  styleUrls: [ './confirm-apply-stop-modal.component.scss' ]
})
export class ConfirmApplyStopModalComponent implements OnChanges {

  @Input() applyRulesStatus: ApplyRulesStatus;

  @Output() confirm: EventEmitter<void> = new EventEmitter();

  @Output() cancel: EventEmitter<void> = new EventEmitter();

  progressValue: number;

  progressPrefixText: string;

  progressSuffixText: string;

  ngOnChanges() {
    const { percentageComplete, estimatedTimeComplete } = this.applyRulesStatus;
    const now = moment();
    const etc = moment(estimatedTimeComplete);
    const dur = moment.duration(etc.diff(now));
    const durHours = Math.floor(dur.get('hours')).toString().padStart(2, '0');
    const durMins = dur.get('minutes').toString().padStart(2, '0');
    const durSecs = dur.get('seconds').toString().padStart(2, '0');
    const durCountdown = etc.diff(now) < 0 ? '00:00:00' : `${durHours}:${durMins}:${durSecs}`;

    this.progressValue = percentageComplete;
    this.progressPrefixText = `${Math.round(percentageComplete * 100)}% complete`;
    this.progressSuffixText = `${durCountdown} until finished`;
  }
}
