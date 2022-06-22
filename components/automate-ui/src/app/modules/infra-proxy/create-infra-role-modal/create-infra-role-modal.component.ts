import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit
} from '@angular/core';
import { Subject, combineLatest } from 'rxjs';
import { Store } from '@ngrx/store';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { takeUntil, filter } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import {
  CreateRole,
  GetRoles,
  RolesPayload,
  CreateRolePayload
} from 'app/entities/infra-roles/infra-role.action';
import {
  saveStatus,
  saveError
} from 'app/entities/infra-roles/infra-role.selectors';
import { isNil } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import { Utilities } from 'app/helpers/utilities/utilities';
import { ListItem } from '../select-box/src/lib/list-item.domain';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { AvailableType } from '../infra-roles/infra-roles.component';

const CREATE_TAB_NAME = 'roleTab';

@Component({
  selector: 'app-create-infra-role-modal',
  templateUrl: './create-infra-role-modal.component.html',
  styleUrls: ['./create-infra-role-modal.component.scss']
})

export class CreateInfraRoleModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<void>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() currentPage: number;
  @Input() availablelist: AvailableType[];

  public constraintsTab = false;
  public conflictError = false;
  public creating = false;
  public defaultAttrParseError = false;
  public defaultTab = false;
  public detailsTab = true;
  public overrideAttrParseError = false;
  public overrideTab = false;
  public showdrag = false;
  public visible = false;
  public attr_value = '{}';
  public page = 1;
  public per_page = 9;
  public org: string;
  public recipes: string[] = [];
  public selectedRunList: string[] = [];
  public server: string;
  public currentRunList: ListItem[] = [];
  public close = new EventEmitter();
  public conflictErrorEvent = new EventEmitter<boolean>();
  public detailsFormGroup: FormGroup;
  public defaultAttrFormGroup: FormGroup;
  public overrideAttrFormGroup: FormGroup;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) {
    this.detailsFormGroup = this.fb.group({
      name: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]],
      description: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.defaultAttrFormGroup = this.fb.group({
      default: ['{}']
    });
    this.overrideAttrFormGroup = this.fb.group({
      override: ['{}', [Validators.required]]
    });
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.conflictError = false;
        this.visible = true;
        this.server = this.serverId;
        this.org = this.orgId;
        this.showdrag = true;
    });

    const payload: RolesPayload = {
      roleName: '',
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.currentPage,
      per_page: this.per_page
    };
    this.store.select(saveStatus)
    .pipe(
      takeUntil(this.isDestroyed),
      filter(state => state === EntityStatus.loadingSuccess))
      .subscribe(state => {
        this.creating = false;
        if (state === EntityStatus.loadingSuccess) {
          this.store.dispatch(new GetRoles(payload));
          this.closeCreateModal();
        }
      });
    combineLatest([
      this.store.select(saveStatus),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
          this.conflictError = true;
          this.defaultTab = false;
          this.detailsTab = true;
          this.constraintsTab = false;
          this.overrideTab = false;
        } else {
          this.store.dispatch(new GetRoles(payload));
          // Close the modal on any other error because it will be displayed in the banner.
          this.closeCreateModal();
        }
        this.creating = false;
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  createRole() {
    this.creating = true;
    const role: CreateRolePayload = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.detailsFormGroup.controls['name'].value,
      description: this.detailsFormGroup.controls['description'].value,
      default_attributes: JSON.parse(this.defaultAttrFormGroup.controls['default'].value),
      override_attributes: JSON.parse(this.overrideAttrFormGroup.controls['override'].value),
      run_list: this.selectedRunList
    };
    this.store.dispatch(new CreateRole({role: role}));
    this.telemetryService.track('InfraServer_Roles_Create');
  }

  dragDropHandler(count: ListItem[]) {
    this.selectedRunList = [];
    count.forEach(c => {
      this.selectedRunList.push(`${c.type}[${c.value}]`);
    });
  }

  handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
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

  tabChange(tab: number): void {
    // Tab indices here correspond with the order of
    // `<app-infra-tab-change>` elements in the template.
    switch (tab) {
      case 0:
        this.telemetryService.track(CREATE_TAB_NAME, 'details');
        this.resetTabs();
        this.detailsTab = true;
        break;
      case 1:
        this.telemetryService.track(CREATE_TAB_NAME, 'constraints');
        this.resetTabs();
        this.constraintsTab = true;
        break;
      case 2:
        this.telemetryService.track(CREATE_TAB_NAME, 'default');
        this.resetTabs();
        this.defaultTab = true;
        break;
      case 3:
        this.telemetryService.track(CREATE_TAB_NAME, 'override');
        this.resetTabs();
        this.overrideTab = true;
        break;
    }
  }

  private resetCreateModal(): void {
    this.selectedRunList = [];
    this.currentRunList = [];
    this.creating = false;
    this.defaultAttrParseError = false;
    this.defaultTab = false;
    this.detailsTab = true;
    this.constraintsTab = false;
    this.overrideTab = false;
    this.overrideAttrParseError = false;
    this.showdrag = false;
    this.detailsFormGroup.reset();
    this.defaultAttrFormGroup.reset();
    this.overrideAttrFormGroup.reset();
    this.defaultAttrFormGroup.controls.default.setValue(this.attr_value);
    this.overrideAttrFormGroup.controls.override.setValue(this.attr_value);
    this.conflictErrorEvent.emit(false);
  }

  private resetTabs(): void {
    this.detailsTab = false;
    this.constraintsTab = false;
    this.defaultTab = false;
    this.overrideTab = false;
  }
}
