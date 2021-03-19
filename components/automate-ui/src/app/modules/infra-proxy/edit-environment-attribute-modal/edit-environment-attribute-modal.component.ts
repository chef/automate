import { Component, EventEmitter, Input, OnInit, OnChanges } from '@angular/core';
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
export class EditEnvironmentAttributeModalComponent implements OnChanges, OnInit {


  @Input() openEvent: EventEmitter<boolean>;
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() jsonText: any;
  @Input() label: string;
  @Input() name: string;
  @Input() cookbookConstraintArray: Array<CookbookConstraintGrid> = [];
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
  public constraintArray: Array<CookbookConstraintGrid> = [];
  public constraintKeys: string[] = [];
  public name_id: string;
  public nameKeys: string[] = [];
  public server: string;
  public org: string;
  public data: any;
  public textareaID: string;
  public selectedCookbookNames: string[] = []; 

  public defaulttAttributeForm: FormGroup;
  public overrideAttributeForm: FormGroup;
  public constraintFormGroup: FormGroup;
  public close = new EventEmitter();
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,

  ) {
    this.defaulttAttributeForm = this.fb.group({
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
      this.showConstraint =  (this.label ===  "Default" || "Override") ? false : true;
      this.selectedCookbookNames = [];
      this.cookbookConstraintArray.forEach((element) => {
        this.selectedCookbookNames.push(element.name);
      });
      this.selectedCookbookNames.forEach((ele) => {
        this.constraintKeys.forEach((element, index) => {
          if(ele === element) {
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
    if(this.label === "Default") {
      this.defaulttAttributeForm.controls.default.setValue(this.jsonText);
    }
    if(this.label === "Override") {
      this.overrideAttributeForm.controls.override.setValue(this.jsonText);
    }
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  capitalizeFirstLetter(name: string) {
    return name.charAt(0).toUpperCase() + name.slice(1);
  }

  closeEditModal(): void {
    this.resetEditModal();
    this.visible = false;
  }

  constraintItemsHandler(value: Array<CookbookConstraintGrid> = []    ) {
    this.constraintArray = value;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.isNavigationKey(event)) {
      this.conflictError = false;
      this.defaulttAttributeForm.controls.default.setValue(
        IdMapper.transform(this.defaulttAttributeForm.controls.default.value.trim()));
    }
  }

  onChangeDefaultJson(event: { target: { value: string } } ) {
    // get value from text area
    const newValue = event.target.value;
    try {
      // parse it to json
      JSON.parse(newValue);
      this.defaultAttrParseError = false;
    } catch (ex) {
      // set parse error if it fails
      this.defaultAttrParseError = true;
    }
  }

  onChangeOverrideJson(event: { target: { value: string } } ) {
    // get value from text area
    const newValue = event.target.value;
    try {
      // parse it to json
      JSON.parse(newValue);
      this.overrideAttrParseError = false;
    } catch (ex) {
      // set parse error if it fails
      this.overrideAttrParseError = true;
    }
  }

  updatingData(environment: Environment) {
    this.store.dispatch(
      new UpdateEnvironment(environment)
    );
  }

  updateEnvironment(): void {
    this.updateSuccessful = false;
    this.updateInProgress = true;

    if(this.label === "Constraints") {

      const environment: Environment = {
        server_id: this.serverId,
        org_id: this.orgId,
        name: this.environment.name,
        description: this.environment.description,
        cookbook_versions: this.constraintArray.length ? this.toDisplay(this.constraintArray) : {},
        default_attributes: JSON.parse(this.environment.default_attributes),
        override_attributes: JSON.parse(this.environment.override_attributes),
        json_class: this.environment.json_class,
        chef_type: this.environment.chef_type
      };
      this.updatingData(environment);
    }

    if(this.label === "Default") {
      let json_data = this.defaulttAttributeForm.controls['default'].value;
      var obj = JSON.parse(json_data.replace(/\r?\n|\r/g, ''));

      const environment: Environment = {
        server_id: this.serverId,
        org_id: this.orgId,
        name: this.environment.name,
        description: this.environment.description,
        cookbook_versions: this.environment.cookbook_versions,
        default_attributes: obj,
        override_attributes: JSON.parse(this.environment.override_attributes),
        chef_type: this.environment.chef_type,
        json_class: this.environment.json_class
      };
      this.updatingData(environment);
    }

    if(this.label === "Override") {
      let json_data = this.overrideAttributeForm.controls['override'].value;
      var obj = JSON.parse(json_data.replace(/\r?\n|\r/g, ''));

      const environment: Environment = {
        server_id: this.serverId,
        org_id: this.orgId,
        name: this.environment.name,
        description: this.environment.description,
        cookbook_versions: this.environment.cookbook_versions,
        default_attributes: JSON.parse(this.environment.default_attributes),
        override_attributes: obj,
        chef_type: this.environment.chef_type,
        json_class: this.environment.json_class
      };
      this.updatingData(environment);
    }
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

      // first cookbook constrains keys selected on drop-down when loading constrain data
      this.name_id = this.constraintKeys[0];
    });

  }

  private toDisplay(cookbookVersions: Array<CookbookConstraintGrid> = []) {
    const current = {};
    cookbookVersions.forEach((element) => {
      current[element.name] =
        `${element.operator}` + ' ' + `${element.version}`;
    });
    return current;
  }

  private resetEditModal(): void {
    this.cookbookConstraintArray = [];
    this.creating = false;
    this.showConstraint = false;
    this.loadCookbooks();
    this.constraintFormGroup.controls.version.setValue('');
    this.cookbookVersions.forEach((obj, index) => {
      this.cookbookConstraintArray.push({
        id: index + 1,
        name: obj.name,
        operator: obj.operator,
        version: obj.versionNumber
      });

    });

    this.conflictErrorEvent.emit(false);
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

}
