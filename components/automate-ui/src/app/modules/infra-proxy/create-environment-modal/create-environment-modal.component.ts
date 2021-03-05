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
import { CreateEnvironment, GetEnvironments } from 'app/entities/environments/environment.action';

const CREATE_TAB_NAME = 'environmentTab';

export class DynamicGrid {
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

  public visible = false;
  public creating = false;
  public conflictError = false;
  public constraintsTab = false;
  public detailsTab = true;
  public isLinear = true;
  public defaultTab = false;
  public overrideTab = false;
  public showdrag = false;
  public showConstraint = false;

  public firstFormGroup: FormGroup;
  public thirdFormGroup: FormGroup;
  public fourthFormGroup: FormGroup;
  public constraintKeysp: string[] = [];
  public cookbooks: Cookbook[] = [];
  public default_attr_value = '{}';
  public override_attr_value = '{}';
  public server: string;
  public org: string;
  public per_page = 9;
  public page = 1;
  public name_idp = '';
  public jsonString: string;
  public dattrParseError: boolean;
  public oattrParseError: boolean;
  public data: any;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public close = new EventEmitter();
  public constraintArray: Array<DynamicGrid> = [];
  public items: Environment[] = [];
  public textareaID: string;
  public dynamicArrayp: Array<DynamicGrid> = [];
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService

  ) {

    this.firstFormGroup = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      description: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });

    this.thirdFormGroup = this.fb.group({
      dattr: ['']
    });

    this.fourthFormGroup = this.fb.group({
      oattr: ['', [Validators.required]]
    });

  }

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
      this.conflictError = false;
      this.visible = true;
      this.items = this.environmentsList;
      this.showdrag = true;
      this.showConstraint = true;
      this.server = this.serverId;
      this.org = this.orgId;
    });

    const payload = {
      environmentName: '',
      page: this.page,
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
        } else {
          this.store.dispatch(new GetEnvironments(payload));
          this.creating = false;

          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
        }
        this.creating = false;
      });
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
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
  constraintItemsHandler(value: Array<DynamicGrid> = []    ) {
    this.constraintArray = value;
  }

  // Getting list of cookbook names 
  loadCookbookConstraint() {
    this.name_idp = '';
    this.store.dispatch(new GetCookbooks({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(allCookbooks)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getCookbooksSt, allCookbooksState]) => {
      if (getCookbooksSt === EntityStatus.loadingSuccess && !isNil(allCookbooksState)) {
        this.constraintKeysp = [];
        this.cookbooks = allCookbooksState;
        this.cookbooks.forEach((cookbook) => {
          this.constraintKeysp.push(cookbook.name);
        });
      }
      this.name_idp = this.constraintKeysp[0];
    });

  }

  createEnvironment() {
    this.creating = true;
    const environment = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.firstFormGroup.controls['name'].value,
      description: this.firstFormGroup.controls['description'].value,
      cookbook_versions: this.constraintArray.length ? this.toDisplay(this.constraintArray) : {},
      default_attributes: JSON.parse(this.thirdFormGroup.controls['dattr'].value),
      override_attributes: JSON.parse(this.fourthFormGroup.controls['oattr'].value)
    };

    this.store.dispatch(
      new CreateEnvironment({
        server_id: this.serverId, org_id: this.orgId, environment: environment
      })
    );
  }

  toDisplay(cookbookVersions: Array<DynamicGrid> = []) {
    const current = {};
    cookbookVersions.forEach((element) => {
      current[element.name] =
        `${element.operator}` + ' ' + `${element.version}`;
    });
    return current;
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  private resetCreateModal(): void {
    this.creating = false;
    this.firstFormGroup.reset();
    this.thirdFormGroup.reset();
    this.fourthFormGroup.reset();

    this.overrideTab = false;
    this.defaultTab = false;
    this.constraintsTab = false;
    this.detailsTab = true;
    this.default_attr_value = '{}';
    this.override_attr_value = '{}';
    this.dattrParseError = false;
    this.oattrParseError = false;
    this.dynamicArrayp = [];
    this.loadCookbookConstraint();
    this.conflictErrorEvent.emit(false);
  }

  private resetTabs() {
    this.detailsTab = false;
    this.constraintsTab = false;
    this.defaultTab = false;
    this.overrideTab = false;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

  // this is the initial value set to the component
  public writeValue(obj: any) {

    if (obj) {
      this.data = obj;
      // this will format it with 4 character spacing
      this.jsonString = JSON.stringify(this.data, undefined, 4);
    }
  }

  // registers 'fn' that will be fired wheb changes are made
  // this is how we emit the changes back to the form
  public registerOnChange(fn: any) {
    this.propagateChange = fn;
  }

  public onChangeJSON(event) {
    this.dattrParseError = false;
    this.oattrParseError = false;
    // get value from text area
    const newValue = event.target.value;
    this.textareaID = event.target.id;
    try {
        // parse it to json
      this.data = JSON.parse(newValue);
      this.textareaID === 'dattr' ? (this.dattrParseError = false) : '';
      this.textareaID === 'oattr' ? (this.oattrParseError = false) : '';
    } catch (ex) {
        // set parse error if it fails
      this.textareaID === 'dattr' ? (this.dattrParseError = true) : '';
      this.textareaID === 'oattr' ? (this.oattrParseError = true) : '';
    }
    // update the form
    this.propagateChange(this.data);
  }

  // the method set in registerOnChange to emit changes back to the form
  private propagateChange = (_: any) => { };

}
