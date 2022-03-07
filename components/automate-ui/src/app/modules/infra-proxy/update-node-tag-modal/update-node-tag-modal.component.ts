import { Component, EventEmitter, Input, OnInit, OnDestroy } from '@angular/core';
import { combineLatest, Subject } from 'rxjs';
import { isNil } from 'lodash/fp';
import { takeUntil } from 'rxjs/operators';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  updateTagsStatus,
  nodeTags
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import {
  UpdateNodeTags
} from 'app/entities/infra-nodes/infra-nodes.actions';
import { EntityStatus } from 'app/entities/entities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-update-node-tag-modal',
  templateUrl: './update-node-tag-modal.component.html',
  styleUrls: ['./update-node-tag-modal.component.scss']
})
export class UpdateNodeTagModalComponent implements OnInit, OnDestroy {

  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() name: string;

  public conflictError = false;
  public visible = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  // tag
  public inputTxt = '';
  public tags: string[] = [];
  public updatingTags = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
      this.conflictError = false;
      this.visible = true;
    });

    combineLatest([
      this.store.select(updateTagsStatus),
      this.store.select(nodeTags)
    ]).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe(([getTagsSt, TagsState]) => {
      if (getTagsSt === EntityStatus.loadingSuccess && !isNil(TagsState)) {
        this.updatingTags = false;
        this.closeEditModal();

      } else if (getTagsSt === EntityStatus.loadingFailure) {
        this.updatingTags = false;
        this.closeEditModal();
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeEditModal(): void {
    this.resetEditModal();
    this.visible = false;
  }

  // update tags
  updateTags(action: string, tags: string[]) {
    this.updatingTags = true;
    const updatedNode = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.name,
      action: action,
      tags: tags
    };
    this.store.dispatch(new UpdateNodeTags({node: updatedNode}));
  }

  addTags() {
    if (this.inputTxt !== '') {
      this.tags = this.tags.concat(this.inputTxt.replace(/^[,\s]+|[,\s]+$/g, '')
        .replace(/,[,\s]*,/g, ',').split(',').map(item => item.trim()));
      this.updateTags('add', this.tags);
      this.telemetryService.track('InfraServer_Nodes_UpdateTags');
    }
  }

  private resetEditModal(): void {
    this.inputTxt = '';
    this.tags = [];
    this.conflictErrorEvent.emit(false);
  }
}
