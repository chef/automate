import {
  Component,
  EventEmitter,
  Input,
  OnInit,
  OnChanges,
  OnDestroy,
  Output
} from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  updateStatus
} from 'app/entities/infra-roles/infra-role-details.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { ListItem } from '../select-box/src/lib/list-item.domain';
import { UpdateRole } from 'app/entities/infra-roles/infra-role.action';
import { Utilities } from 'app/helpers/utilities/utilities';
import { AvailableType } from '../infra-roles/infra-roles.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-edit-infra-role-modal',
  templateUrl: './edit-infra-role-modal.component.html',
  styleUrls: ['./edit-infra-role-modal.component.scss']
})
export class EditInfraRoleModalComponent implements OnChanges, OnInit, OnDestroy {
  @Input() jsonText: any;
  @Input() label: string;
  @Input() openEvent: EventEmitter<boolean>;
  @Input() orgId: string;
  @Input() availableType: AvailableType[] = [];
  @Input() role: InfraRole;
  @Input() serverId: string;
  @Input() selected: ListItem[] = [];
  @Output() runlistUpdated: EventEmitter<void> =   new EventEmitter();

  public creating = false;
  public conflictError = false;
  public defaultAttrParseError = false;
  public isLoading = true;
  public isRunlist = false;
  public showbutton = false;
  public overrideAttrParseError = false;
  public updateSuccessful = false;
  public updateInProgress = false;
  public visible = false;
  public server: string;
  public org: string;
  public selectedRunLists: string[] = [];
  public currentRunList: ListItem[] = [];
  public current_Page = 0;
  public availablelist: AvailableType[] = [];
  public defaultAttributeForm: FormGroup;
  public overrideAttributeForm: FormGroup;
  public close = new EventEmitter();
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {
    this.defaultAttributeForm = this.fb.group({
      default: ['', [Validators.required]]
    });
    this.overrideAttributeForm = this.fb.group({
      override: ['', [Validators.required]]
    });
  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
      this.conflictError = false;
      this.showbutton = true;
      this.visible = true;
      this.server = this.serverId;
      this.org = this.orgId;
      this.currentRunList = [];
      this.current_Page = 0;
      this.selected.forEach((element) => {
        element.selected = false;
        this.currentRunList.push(element);
      });
      this.availablelist = [];
      this.availableType.forEach((element) => {
        this.availablelist.push(element);
      });
    });
    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([status]) => {
      this.isLoading = status === EntityStatus.loading;
    });
    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.updateInProgress && !pending(state)))
      .subscribe((state) => {
        this.updateInProgress = false;
        this.updateSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.updateSuccessful) {
          this.runlistUpdated.emit();
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
  }

  dragDropHandler(runlists: ListItem[]) {
    this.selectedRunLists = [];
    runlists.forEach(runlist => {
      this.selectedRunLists.push(`${runlist.type}[${runlist.value}]`);
    });
    this.isRunlist = true;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.defaultAttributeForm.controls.default.setValue(
        IdMapper.transform(this.defaultAttributeForm.controls.default.value.trim()));
    }
  }

  onChangeDefaultJson(event: { target: { value: string } } ) {
    const newValue = event.target.value;
    try {
      JSON.parse(newValue);
      this.defaultAttrParseError = false;
    } catch (ex) {
      this.defaultAttrParseError = true;
    }
  }

  onChangeOverrideJson(event: { target: { value: string } } ) {
    const newValue = event.target.value;
    try {
      JSON.parse(newValue);
      this.overrideAttrParseError = false;
    } catch (ex) {
      this.overrideAttrParseError = true;
    }
  }

  updateInfrarole(): void {
    this.updateSuccessful = false;
    this.updateInProgress = true;

    let role: InfraRole = {
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.role.name,
      description: this.role.description,

      // these are filled in by the switch below
      run_list: [],
      default_attributes: '',
      override_attributes: ''
    };

    switch (this.label) {
      case 'Run List':
        role = { ...role,
          run_list: this.isRunlist ? this.selectedRunLists : this.role.run_list,
          default_attributes: JSON.parse(this.role.default_attributes),
          override_attributes: JSON.parse(this.role.override_attributes)
        };
        this.telemetryService.track('InfraServer_Roles_EditRunList');
        break;

      case 'Default':
        role = { ...role,
          run_list: this.role.run_list,
          default_attributes: JSON.parse(
            this.defaultAttributeForm.controls['default'].value.replace(/\r?\n|\r/g, '')),
          override_attributes: JSON.parse(this.role.override_attributes)
        };
        this.telemetryService.track('InfraServer_Roles_EditDefault');
        break;

      case 'Override':
        role = { ...role,
          run_list: this.selectedRunLists.length > 0 ? this.selectedRunLists : this.role.run_list,
          default_attributes: JSON.parse(this.role.default_attributes),
          override_attributes: JSON.parse(
            this.overrideAttributeForm.controls['override'].value.replace(/\r?\n|\r/g, ''))
        };
        this.telemetryService.track('InfraServer_Roles_EditOverride');
        break;
    }
    this.updatingData(role);
  }

  private resetEditModal(): void {
    this.creating = false;
    this.defaultAttributeForm.markAsPristine();
    this.overrideAttributeForm.markAsPristine();
    this.selectedRunLists = [];
    this.defaultAttrParseError = false;
    this.overrideAttrParseError = false;
    this.showbutton = false;
    this.isRunlist = false;
    this.setAttributeValue();
    this.conflictErrorEvent.emit(false);
  }

  private setAttributeValue() {
    if (this.label === 'Default') {
      this.defaultAttributeForm.controls.default.setValue(this.jsonText);
    }
    if (this.label === 'Override') {
      this.overrideAttributeForm.controls.override.setValue(this.jsonText);
    }
  }

  private updatingData(role: InfraRole) {
    this.store.dispatch(new UpdateRole(role));
  }
}
