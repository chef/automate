import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit,
  ViewChild
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
import { MatStepper } from '@angular/material/stepper';
import { Environment } from 'app/entities/environments/environment.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { GetCookbooks } from 'app/entities/cookbooks/cookbook.actions';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import {
  allCookbooks,
  getAllStatus as getAllCookbooksForOrgStatus
} from 'app/entities/cookbooks/cookbook.selectors';
import { MatSelect } from '@angular/material/select';
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

  @Input() openEvent: EventEmitter<boolean>;
  @Input() environmentsList: Environment[] = [];
  @Input() serverId: string;
  @Input() orgId: string;

  @ViewChild('stepper') stepper: MatStepper;
  @ViewChild(MatSelect) select: MatSelect;

  public visible = false;
  public creating = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public close = new EventEmitter();
  public conflictError = false;
  public isLinear = true;
  public detailsTab = true;
  public constraintsTab = false;
  public cookbooks: Cookbook[] = [];
  public defaultTab = false;
  public overrideTab = false;
  public firstFormGroup: FormGroup;
  public thirdFormGroup: FormGroup;
  public fourthFormGroup: FormGroup;
  public constraintKeys: string[] = [];
  public server: string;
  public org: string;
  public showdrag = false;
  public per_page = 9;
  public page = 1;
  public jsonString: string;
  public dattrParseError: boolean;
  public oattrParseError: boolean;
  public data: any;
  public textareaID: string;
  public default_attr_value = '{}';
  public override_attr_value = '{}';
  private isDestroyed = new Subject<boolean>();
  constraintArray: Array<DynamicGrid> = [];
  public showConstraint = false;
  items: Environment[] = [];

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


    this.store.dispatch(new GetCookbooks({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(allCookbooks)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getCookbooksSt, allCookbooksState]) => {
      if (getCookbooksSt === EntityStatus.loadingSuccess && !isNil(allCookbooksState)) {
        this.cookbooks = allCookbooksState;
        this.cookbooks.forEach((cookbook) => {
          this.constraintKeys.push(cookbook.name);
        });
      }
    });




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
          this.stepper.selectedIndex = 0;
        } else {
          this.store.dispatch(new GetEnvironments(payload));

          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
        }
        this.creating = false;
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }


  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  constraintItemsHandler(value: Array<DynamicGrid> = []    ) {
    this.constraintArray = value;


    // this.valueChanged.emit(this.selected);
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  tabChange(tab: number) {
    // Tab indices here correspond with the order of `<app-tab>` elements in the template.
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

  private resetTabs() {
    this.detailsTab = false;
    this.constraintsTab = false;
    this.defaultTab = false;
    this.overrideTab = false;
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
// const value = {
// };
    // const value = {}
    // return Object.keys(cookbookVersions).map(function (key) {
    //   const value = cookbookVersions[key].split(' ');
    //   return {name: key, operator: value[0], versionNumber: value[1]};
    // });
  }

  private resetCreateModal(): void {
    this.creating = false;
    this.firstFormGroup.reset();
    this.overrideTab = false;
    this.defaultTab = false;
    this.constraintsTab = false;
    this.detailsTab = true;
    //this.resetTabs();
    //this.secondFormGroup.reset();
    this.default_attr_value = '{}';
    this.override_attr_value = '{}';
    // this.stepper.selectedIndex = 0;
    this.conflictErrorEvent.emit(false);
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
