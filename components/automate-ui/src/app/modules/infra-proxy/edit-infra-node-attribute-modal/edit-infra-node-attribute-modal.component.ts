import { Component, EventEmitter, Input, OnInit, OnChanges, OnDestroy } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  updateAttributesStatus
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { InfraNodeAttribute } from 'app/entities/infra-nodes/infra-nodes.model';
import { UpdateNodeAttributes, GetNode } from 'app/entities/infra-nodes/infra-nodes.actions';
import { Utilities } from 'app/helpers/utilities/utilities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-edit-infra-node-attribute-modal',
  templateUrl: './edit-infra-node-attribute-modal.component.html',
  styleUrls: ['./edit-infra-node-attribute-modal.component.scss']
})
export class EditInfraNodeAttributeModalComponent implements OnChanges, OnInit, OnDestroy {

  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() jsonText: any;
  @Input() label: string;
  @Input() name: string;
  @Input() node: InfraNodeAttribute;
  @Input() isGetNode: boolean;

  public creating = false;
  public conflictError = false;
  public attrParseError = false;
  public isLoading = true;
  public visible = false;
  public updateSuccessful = false;
  public updateInProgress = false;

  public server: string;
  public org: string;
  public data: any;
  public textareaID: string;

  public attributeForm: FormGroup;
  public close = new EventEmitter();
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {
    this.attributeForm = this.fb.group({
      default: ['', [Validators.required]]
    });
  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
      this.conflictError = false;
      this.visible = true;
      this.server = this.serverId;
      this.org = this.orgId;
     });

    this.store.select(updateAttributesStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.updateInProgress && !pending(state)))
      .subscribe((state) => {
        this.updateInProgress = false;
        this.updateSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.updateSuccessful) {
          this.closeEditModal();
        }
      });
  }

  ngOnChanges(): void {
    this.setAttributeValue();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeEditModal(): void {
    this.resetEditModal();
    this.visible = false;
    if (this.isGetNode === true) {
      this.getNode();
    }
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.attributeForm.controls.default.setValue(
        IdMapper.transform(this.attributeForm.controls.default.value.trim()));
    }
  }

  onChangeDefaultJson(event: { target: { value: string } } ) {
    const newValue = event.target.value;
    try {
      JSON.parse(newValue);
      this.attrParseError = false;
    } catch (ex) {
      this.attrParseError = true;
    }
  }

  updateNodeAttribute(): void {
    this.updateSuccessful = false;
    this.updateInProgress = true;

    let nodeAttr = {
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.node.name,
      attributes: []
    };

    nodeAttr = { ...nodeAttr,
      attributes: JSON.parse(
          this.attributeForm.controls['default'].value.replace(/\r?\n|\r/g, ''))
    };

    this.store.dispatch(new UpdateNodeAttributes(nodeAttr));
    this.telemetryService.track('InfraServer_Nodes_EditAttributes');
  }

  private resetEditModal(): void {
    this.creating = false;
    this.attributeForm.markAsPristine();
    this.attrParseError = false;
    this.setAttributeValue();
    this.conflictErrorEvent.emit(false);
  }

  private setAttributeValue() {
    this.attributeForm.controls.default.setValue(this.jsonText);
  }

  public getNode() {
    this.store.dispatch(new GetNode({
      server_id: this.serverId, org_id: this.orgId, name: this.node.name
    }));
  }
}
