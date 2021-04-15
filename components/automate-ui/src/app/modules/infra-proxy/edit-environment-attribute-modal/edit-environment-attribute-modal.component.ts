import { Component, EventEmitter, Input, OnInit, OnChanges, OnDestroy } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { combineLatest, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { Environment, CookbookVersionDisplay } from 'app/entities/environments/environment.model';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  updateStatus
} from 'app/entities/environments/environment-details.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { UpdateEnvironment } from 'app/entities/environments/environment.action';
import { GetCookbooks } from 'app/entities/cookbooks/cookbook.actions';
import {
  allCookbooks,
  getAllStatus as getAllCookbooksForOrgStatus
} from 'app/entities/cookbooks/cookbook.selectors';
import { isNil } from 'ngx-cookie';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import { Regex } from 'app/helpers/auth/regex';
import { Utilities } from 'app/helpers/utilities/utilities';

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

  public creating = false;
  public conflictError = false;
  public defaultAttrParseError = false;
  public isLoading = true;
  public overrideAttrParseError = false;
  public showConstraint = true;
  public visible = false;
  public updateSuccessful = false;
  public updateInProgress = false;

  public attrParseError: boolean;
  public cookbooks: Cookbook[] = [];
  public constraints: Array<CookbookConstraintGrid> = [];
  public constraintKeys: string[] = [];
  public name_id: string;
  public nameKeys: string[] = [];
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
    private store: Store<NgrxStateAtom>

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
      this.visible = true;
      this.server = this.serverId;
      this.org = this.orgId;
      this.showConstraint =  (this.label ===  'Default' || 'Override') ? false : true;
      this.selectedCookbookNames = [];
      this.cookbookConstraints.forEach((element) => {
        this.selectedCookbookNames.push(element.name);
      });
      this.selectedCookbookNames.forEach((cookbookName) => {
        this.constraintKeys.forEach((key, index) => {
          if (cookbookName === key) {
            this.constraintKeys.splice(index, 1);
          }
        });
      });
      this.name_id = this.constraintKeys[0];

    });

    this.loadCookbooks();

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
    if (this.label === 'Default') {
      this.defaultAttributeForm.controls.default.setValue(this.jsonText);
    }
    if (this.label === 'Override') {
      this.overrideAttributeForm.controls.override.setValue(this.jsonText);
    }
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeEditModal(): void {
    this.resetEditModal();
    this.visible = false;
  }

  constraintItemsHandler(value: Array<CookbookConstraintGrid> = []    ) {
    this.constraints = value;
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
          cookbook_versions: this.constraints.length ? this.toDisplay(this.constraints) : {},
          default_attributes: JSON.parse(this.environment.default_attributes),
          override_attributes: JSON.parse(this.environment.override_attributes)
        };
        break;

      case 'Default':
        environment = { ...environment,
          cookbook_versions: this.environment.cookbook_versions,
          default_attributes: JSON.parse(
              this.defaultAttributeForm.controls['default'].value.replace(/\r?\n|\r/g, '')),
          override_attributes: JSON.parse(this.environment.override_attributes)
        };
        break;

      case 'Override':
        environment = { ...environment,
          cookbook_versions: this.environment.cookbook_versions,
          default_attributes: JSON.parse(this.environment.default_attributes),
          override_attributes: JSON.parse(
            this.overrideAttributeForm.controls['override'].value.replace(/\r?\n|\r/g, ''))
        };
        break;
    }
    this.updatingData(environment);
  }

  private loadCookbooks() {
    this.name_id = '';
    this.store.dispatch(new GetCookbooks({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(allCookbooks)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getCookbooksSt, allCookbooksState]) => {
      if (getCookbooksSt === EntityStatus.loadingSuccess && !isNil(allCookbooksState)) {
        this.constraintKeys = [];
        this.nameKeys = [];

        this.cookbooks = allCookbooksState;
        this.cookbooks.forEach((cookbook) => {
          this.constraintKeys.push(cookbook.name);
          this.nameKeys.push(cookbook.name);
        });
      }

      // first cookbook constrains keys selected on drop-down when loading constraint data
      this.name_id = this.constraintKeys[0];
    });

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
    this.creating = false;
    this.showConstraint = false;
    this.loadCookbooks();
    this.constraintFormGroup.controls.version.setValue('');
    this.cookbookVersions.forEach((obj, index) => {
      this.cookbookConstraints.push({
        id: index + 1,
        name: obj.name,
        operator: obj.operator,
        version: obj.versionNumber
      });

    });

    this.conflictErrorEvent.emit(false);
  }

  private updatingData(environment: Environment) {
    this.store.dispatch(
      new UpdateEnvironment(environment)
    );
  }

}
