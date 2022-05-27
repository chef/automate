import {
  Component,
  Input,
  OnDestroy,
  EventEmitter,
  Output,
  HostBinding
} from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';

import { GetRevisions } from 'app/entities/revisions/revision.action';
import {
  allRevisions,
  getAllStatus as getAllRevisionsForOrgStatus
} from 'app/entities/revisions/revision.selectors';
import { Revision } from 'app/entities/revisions/revision.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-revision-id',
  templateUrl: './revision-id.component.html',
  styleUrls: ['./revision-id.component.scss']
})

export class RevisionIdComponent implements OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() policyfileName: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();

  public revisions: Revision[] = [];
  public revisionsListLoading = true;
  @HostBinding('class.active') isSlideOpen = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) { }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeRevisionId() {
    this.toggleSlide();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel(policyfile: string) {
    this.policyfileName = policyfile;
    this.isSlideOpen = true;
    this.loadRevisions(this.policyfileName);
    this.telemetryService.track('InfraServer_PolicyFiles_Revisions');
  }

  private loadRevisions(policyfileName): void {
    this.store.dispatch(new GetRevisions({
      server_id: this.serverId, org_id: this.orgId, name: policyfileName
    }));
    combineLatest([
      this.store.select(getAllRevisionsForOrgStatus),
      this.store.select(allRevisions)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getRevisionsSt, allRevisionsState]) => {
        if (getRevisionsSt === EntityStatus.loadingSuccess && !isNil(allRevisionsState)) {
          this.revisions = allRevisionsState;
          this.revisionsListLoading = false;
        } else if (getRevisionsSt === EntityStatus.loadingFailure) {
          this.revisionsListLoading = false;
        }
      });
  }

}
