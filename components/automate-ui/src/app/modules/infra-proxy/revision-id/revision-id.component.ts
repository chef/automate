import { Component, Input, OnInit, OnDestroy, EventEmitter, Output, OnChanges, SimpleChanges } from '@angular/core';
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

@Component({
  selector: 'app-revision-id',
  templateUrl: './revision-id.component.html',
  styleUrls: ['./revision-id.component.scss']
})

export class RevisionIdComponent implements OnInit, OnDestroy, OnChanges {
  @Input() openEvent: EventEmitter<void>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() policyfileName: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();
  @Output() close = new EventEmitter();

  private isDestroyed = new Subject<boolean>();

  public visible = false;
  public revisions: Revision[] = [];
  public revisionsListLoading = true;

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.visible = true;
    });
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes?.policyfileName?.currentValue) {
      this.loadRevisions(changes.policyfileName.currentValue);
    }
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeRevisionId() {
    this.close.emit();
    this.visible = false;
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
