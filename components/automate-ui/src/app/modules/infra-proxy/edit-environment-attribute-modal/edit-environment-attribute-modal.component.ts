import { Component, EventEmitter, Input, OnInit, OnChanges, OnDestroy } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { Environment, CookbookVersionDisplay } from 'app/entities/environments/environment.model';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  updateStatus
} from 'app/entities/environments/environment-details.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { UpdateEnvironment } from 'app/entities/environments/environment.action';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import { Regex } from 'app/helpers/auth/regex';
import { Utilities } from 'app/helpers/utilities/utilities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

export class CookbookConstraintGrid {
  id: number;
  name: string;
  operator: string;
  version: string;
}

@Component({
  selector: 'app-edit-environment-attribute-modal',
  templateUrl: './edit-environment-attribute-modal.component.html',
  styleUrls: ['./edit-environment-attribute-modal.component.scss']
})
export class EditEnvironmentAttributeModalComponent implements OnChanges, OnInit, OnDestroy {

  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() jsonText: any;
  @Input() label: string;
  @Input() name: string;
  @Input() cookbookConstraints: Array<CookbookConstraintGrid> = [];
  @Input() cookbookVersions: CookbookVersionDisplay[];
  @Input() environment: Environment;
  @Input() constraintKeys: string[] = [];
  @Input() name_id: string;
  @Input() nameKeys: string[] = [];

  public creating = false;
  public conflictError = false;
  public cookbookVersionError = false;
  public defaultAttrParseError = false;
  public isLoading = true;
  public overrideAttrParseError = false;
  public showConstraint = true;
  public visible = false;
  public updateSuccessful = false;
  public updateInProgress = false;
  public isConstraints = false;

  public attrParseError: boolean;
  public cookbooks: Cookbook[] = [];
  public constraints: Array<CookbookConstraintGrid> = [];
  public server: string;
  public org: string;
  public data: any;
  public textareaID: string;
  public selectedCookbookNames: string[] = [];

  public defaultAttributeForm: FormGroup;
  public overrideAttributeForm: FormGroup;
  public constraintFormGroup: FormGroup;
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

    this.constraintFormGroup = this.fb.group({
      version: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_VERSION)]]
    });

  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
      this.conflictError = false;
      this.cookbookVersionError = false;
      this.visible = true;
      this.server = this.serverId;
      this.org = this.orgId;
      this.showConstraint =  (this.label ===  'Default' || 'Override') ? false : true;
      this.selectedCookbookNames = [];
      this.cookbookConstraints.forEach((element) => {
        this.selectedCookbookNames.push(element.name);
      });
      this.cookbookConstraints.forEach((cookbookName) => {
        if (!this.constraintKeys.includes(cookbookName.name)) {
          this.constraintKeys.push(cookbookName.name);
        }
        if (!this.nameKeys.includes(cookbookName.name)) {
          this.nameKeys.push(cookbookName.name);
        }
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

  constraintItemsHandler(values: Array<CookbookConstraintGrid> = []) {
    for ( const value of values ) {
      if (!Regex.patterns.VALID_VERSION.test(value.version)) {
        this.cookbookVersionError = true;
        break;
      } else {
        this.cookbookVersionError = false;
      }
    }
    if (!this.cookbookVersionError) {
      this.constraints = values;
      this.isConstraints = true;
    }
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

  updateEnvironment(): void {
    this.updateSuccessful = false;
    this.updateInProgress = true;

    let environment: Environment = {
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.environment.name,
      description: this.environment.description,
      json_class: this.environment.json_class,
      chef_type: this.environment.chef_type,

      // these are filled in by the switch below
      cookbook_versions: {},
      default_attributes: '',
      override_attributes: ''
    };

    switch (this.label) {
      case 'Constraints':
        environment = { ...environment,
          cookbook_versions: this.isConstraints ? this.toDisplay(this.constraints) : {},
          default_attributes: JSON.parse(this.environment.default_attributes),
          override_attributes: JSON.parse(this.environment.override_attributes)
        };
        this.telemetryService.track('InfraServer_Environments_EditConstraints');
        break;

      case 'Default':
        environment = { ...environment,
          cookbook_versions: this.environment.cookbook_versions,
          default_attributes: JSON.parse(
              this.defaultAttributeForm.controls['default'].value.replace(/\r?\n|\r/g, '')),
          override_attributes: JSON.parse(this.environment.override_attributes)
        };
        this.telemetryService.track('InfraServer_Environments_EditDefault');
        break;

      case 'Override':
        environment = { ...environment,
          cookbook_versions: this.environment.cookbook_versions,
          default_attributes: JSON.parse(this.environment.default_attributes),
          override_attributes: JSON.parse(
            this.overrideAttributeForm.controls['override'].value.replace(/\r?\n|\r/g, ''))
        };
        this.telemetryService.track('InfraServer_Environments_EditOverride');
        break;
    }
    this.updatingData(environment);
  }

  private toDisplay(cookbookVersions: Array<CookbookConstraintGrid> = []) {
    const current = {};
    cookbookVersions.forEach((element) => {
      current[element.name] = `${element.operator} ${element.version}`;
    });
    return current;
  }

  private resetEditModal(): void {
    this.cookbookConstraints = [];
    this.cookbookVersionError = false;
    this.creating = false;
    this.defaultAttributeForm.markAsPristine();
    this.overrideAttributeForm.markAsPristine();
    this.defaultAttrParseError = false;
    this.overrideAttrParseError = false;
    this.showConstraint = false;
    this.isConstraints = false;
    this.constraintFormGroup.controls.version.setValue('');
    this.selectedCookbookNames = [];
    this.cookbookVersions.forEach((obj, index) => {
      this.cookbookConstraints.push({
        id: index + 1,
        name: obj.name,
        operator: obj.operator,
        version: obj.versionNumber
      });

    });
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

  private updatingData(environment: Environment) {
    this.store.dispatch(
      new UpdateEnvironment(environment)
    );
  }

}
