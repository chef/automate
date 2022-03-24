import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  OnDestroy,
  OnInit,
  Output
} from '@angular/core';
import { Store } from '@ngrx/store';
import { FormBuilder, FormGroup } from '@angular/forms';
import { takeUntil, filter } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { pending, EntityStatus } from 'app/entities/entities';
import { Subject } from 'rxjs';
import { UpdateDataBagItem } from 'app/entities/data-bags/data-bag-details.actions';
import { updateStatus } from 'app/entities/data-bags/data-bag-details.selector';
import { Utilities } from 'app/helpers/utilities/utilities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-edit-data-bag-item-modal',
  templateUrl: './edit-data-bag-item-modal.component.html',
  styleUrls: ['./edit-data-bag-item-modal.component.scss']
})

export class EditDataBagItemModalComponent implements OnChanges, OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() dataBagName: string;
  @Input() dataBagItemName: string;
  @Input() itemDataJson: string;
  @Output() refreshData = new EventEmitter<string>();

  public updating = false;
  public conflictError = false;
  public updateForm: FormGroup;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public visible = false;
  public itemAttrParseError = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {
    this.updateForm = this.fb.group({
      data: [this.itemDataJson]
    });
  }

  ngOnChanges() {
    this.updateForm.controls.data.setValue(this.itemDataJson);
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
      this.conflictError = false;
      this.visible = true;
    });

    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => !pending(state) ))
      .subscribe((state) => {
        this.updating = false;
        if (state === EntityStatus.loadingSuccess) {
          this.refreshData.emit(this.updateForm.controls['data'].value);
        }
        this.closeCreateModal();
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeCreateModal(): void {
    this.updateForm.controls.data.setValue(this.itemDataJson);
    this.updateForm.markAsPristine();
    this.itemAttrParseError = false;
    this.visible = false;
  }

  updateDataBagItem(): void {
    this.updating = true;
    const data = JSON.parse(this.updateForm.controls['data'].value);
    const dataBagItem = {
      server_id: this.serverId,
      org_id: this.orgId,
      data_bag_name: this.dataBagName,
      name: this.dataBagItemName,
      data: data
    };
    this.store.dispatch(new UpdateDataBagItem({dataBagItem: dataBagItem}));
    this.telemetryService.track('InfraServer_Databags_Details_EditDatabagItem');
  }

  onChangeJSON(event: { target: { value: string } }) {
    // get value from text area
    const newValue = event.target.value;
    try {
      // parse it to json
      JSON.parse(newValue);
      this.itemAttrParseError = false;
    } catch (ex) {
      // set parse error if it fails
      this.itemAttrParseError = true;
    }
  }

}
