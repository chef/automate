import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit
} from '@angular/core';
import { Subject, combineLatest } from 'rxjs';
import { Store } from '@ngrx/store';
import { FormBuilder,  Validators, FormGroup } from '@angular/forms';
import {  takeUntil, filter } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import {
    saveStatus,
    saveError
} from 'app/entities/environments/environment.selectors';
import { isNil } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import { Environment } from 'app/entities/environments/environment.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { GetCookbooks } from 'app/entities/cookbooks/cookbook.actions';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import {
  allCookbooks,
  getAllStatus as getAllCookbooksForOrgStatus
} from 'app/entities/cookbooks/cookbook.selectors';
import { CreateEnvironment, GetEnvironments, GetEnvironmentsPayload, CreateEnvironmentPayload } from 'app/entities/environments/environment.action';
import { Utilities } from 'app/helpers/utilities/utilities';

const CREATE_TAB_NAME = 'environmentTab';

export class CookbookConstraintGrid {
  id: number;
  name: string;
  operator: string;
  version: string;
}

@Component({
  selector: 'app-create-environment-modal',
  templateUrl: './create-environment-modal.component.html',
  styleUrls: ['./create-environment-modal.component.scss']
})

export class CreateEnvironmentModalComponent implements OnInit, OnDestroy {

  @Input() openEvent: EventEmitter<void>;
  @Input() environmentsList: Environment[] = [];
  @Input() serverId: string;
  @Input() orgId: string;
  @Input() currentPage: number;

  public conflictError = false;
  public cookbookVersionError = false;
  public constraintsTab = false;
  public creating = false;
  public defaultAttrParseError = false;
  public defaultTab = false;
  public detailsTab = true;
  public overrideAttrParseError = false;
  public overrideTab = false;
  public showConstraint = false;
  public visible = false;

  public attr_value = '{}';
  public constraints: Array<CookbookConstraintGrid> = [];
  public constraintKeys: string[] = [];
  public cookbooks: Cookbook[] = [];
  public cookbookConstraints: Array<CookbookConstraintGrid> = [];
  public items: Environment[] = [];
  public name_id = '';
  public nameKeys: string[] = [];
  public per_page = 9;
  public org: string;
  public selectedCookbookNames: string[] = [];
  public server: string;

  public close = new EventEmitter();
  public conflictErrorEvent = new EventEmitter<boolean>();
  public constraintFormGroup: FormGroup;
  public detailsFormGroup: FormGroup;
  public defaultAttrFormGroup: FormGroup;
  public overrideAttrFormGroup: FormGroup;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService

  ) {

    this.constraintFormGroup = this.fb.group({
      version: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_VERSION)]]
    });

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

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
      this.conflictError = false;
      this.cookbookVersionError = false;
      this.visible = true;
      this.items = this.environmentsList;
      this.showConstraint = true;
      this.selectedCookbookNames = [];
      this.server = this.serverId;
      this.org = this.orgId;
    });

    const payload: GetEnvironmentsPayload = {
      environmentName: '',
      page: this.currentPage,
      per_page: this.per_page,
      server_id: this.serverId,
      org_id: this.orgId
    };
    this.loadCookbookConstraint();

    this.store.select(saveStatus)
    .pipe(
      takeUntil(this.isDestroyed),
      filter(state => state === EntityStatus.loadingSuccess))
      .subscribe(state => {
        this.creating = false;
        if (state === EntityStatus.loadingSuccess) {
          this.store.dispatch(new GetEnvironments(payload));
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
          this.store.dispatch(new GetEnvironments(payload));
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

  createEnvironment() {
    this.creating = true;
    const environment: CreateEnvironmentPayload = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.detailsFormGroup.controls['name'].value,
      description: this.detailsFormGroup.controls['description'].value,
      cookbook_versions: this.constraints.length ? this.toDisplay(this.constraints) : {},
      default_attributes: JSON.parse(this.defaultAttrFormGroup.controls['default'].value),
      override_attributes: JSON.parse(this.overrideAttrFormGroup.controls['override'].value)
    };

    this.store.dispatch(
      new CreateEnvironment({environment: environment})
    );
    this.telemetryService.track('InfraServer_Environments_Create');
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

  tabChange(tab: number) {
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


  // Handles the data of cookbook version array coming from constraint tab.
  constraintItemsHandler(values: Array<CookbookConstraintGrid> = []    ) {
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
    }
  }

  // Getting list of cookbook names
  private loadCookbookConstraint() {
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

  toDisplay(cookbookVersions: Array<CookbookConstraintGrid> = []) {
    const current = {};
    cookbookVersions.forEach((element) => {
      current[element.name] =
        `${element.operator}` + ' ' + `${element.version}`;
    });
    return current;
  }

  private resetCreateModal(): void {
    this.cookbookConstraints = [];
    this.constraintsTab = false;
    this.cookbookVersionError = false;
    this.creating = false;
    this.defaultAttrParseError = false;
    this.defaultTab = false;
    this.detailsTab = true;
    this.overrideTab = false;
    this.overrideAttrParseError = false;
    this.showConstraint = false;

    this.detailsFormGroup.reset();
    this.defaultAttrFormGroup.reset();
    this.overrideAttrFormGroup.reset();

    this.constraintFormGroup.controls.version.setValue('');
    this.defaultAttrFormGroup.controls.default.setValue(this.attr_value);
    this.overrideAttrFormGroup.controls.override.setValue(this.attr_value);
    this.loadCookbookConstraint();
    this.conflictErrorEvent.emit(false);
  }

  private resetTabs() {
    this.detailsTab = false;
    this.constraintsTab = false;
    this.defaultTab = false;
    this.overrideTab = false;
  }

}
