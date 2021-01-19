import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit
} from '@angular/core';
import { Subject } from 'rxjs';
import { Store } from '@ngrx/store';
import { FormBuilder,  Validators, FormGroup } from '@angular/forms';
import { filter, takeUntil } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { InfraRole, RoleAttributes } from 'app/entities/infra-roles/infra-role.model';
import { CreateRole, GetRoles } from 'app/entities/infra-roles/infra-role.action';
import {
    saveStatus,
    saveError
  } from 'app/entities/infra-roles/infra-role.selectors';
  import { isNil } from 'lodash/fp';
  import {  EntityStatus } from 'app/entities/entities';
  import { HttpStatus } from 'app/types/types';

import { combineLatest } from 'rxjs';
import { ListItem } from '../select-box/src/lib/list-item.domain';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-create-infra-role-modal',
  templateUrl: './create-infra-role-modal.component.html',
  styleUrls: ['./create-infra-role-modal.component.scss']
})
export class CreateInfraRoleModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<void>;
  @Input() rolesList: InfraRole[] = [];
  @Input() serverId: string;
  @Input() orgId: string;
  public visible = false;
  public creating = false;
  public sending = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public close = new EventEmitter();
  public createForm: FormGroup;
  public hookStatus = UrlTestState.Inactive;
  public conflictError = false;
  public urlState = UrlTestState;
  public targetKeys: string[];
  public alertTypeKeys: string[];
  public selectedItems: string[] = [];

  public isLinear = true;
  public firstFormGroup: FormGroup;
  public secondFormGroup: FormGroup;
  public thirdFormGroup: FormGroup;
  public fourthFormGroup: FormGroup;

  basket = [];

  baskets = [];
  public server: string;
  public org: string;
  public showdrag = false;
  items: InfraRole[] = [];
  private isDestroyed = new Subject<boolean>();
  public jsonString: string;
  public dattrParseError: boolean;
  public oattrParseError: boolean;
  public data: any;
  public textareaID: string;
  public default_attr_value = '{}';
  public override_attr_value = '{}';
  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
  ) {

    this.firstFormGroup = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      description: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.secondFormGroup = this.fb.group({

    });
    this.thirdFormGroup = this.fb.group({
      dattr: [''],
    });
    this.fourthFormGroup = this.fb.group({
      oattr: ['',[Validators.required]]
    });
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.conflictError = false;
        this.visible = true;
        this.items = this.rolesList;

      this.showdrag = true;
      this.server = this.serverId;
      this.org = this.orgId;

      });


      this.store.select(saveStatus)
      .pipe(
        takeUntil(this.isDestroyed),
        filter(state => state === EntityStatus.loadingSuccess))
        .subscribe(state => {
          this.creating = false;
          if (state === EntityStatus.loadingSuccess) {
            this.store.dispatch(new GetRoles({
              server_id: this.serverId, org_id: this.orgId
            }));
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
          } else {
            this.creating = false;
            this.store.dispatch(new GetRoles({
              server_id: this.serverId, org_id: this.orgId
            }));
            // Close the modal on any error other than conflict and display in banner.
            this.closeCreateModal();
          }
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

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  dragDropHandler(count: ListItem[]) {
    this.selectedItems = [];
    count.forEach(c =>{
      this.selectedItems.push(`${c.type}[${c.value}]`);
    })

  }

  createRole() {
    this.creating = true;
    const role = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.firstFormGroup.controls['name'].value,
      description: this.firstFormGroup.controls['description'].value,
      default_attributes: {},
      override_attributes: {},
      run_list: [],
      env_run_lists: [
    
      ]
    };
    this.store.dispatch(new CreateRole({server_id: this.serverId, org_id: this.orgId, role: role}));
  }

  createRoleSecond() {
    this.creating = true;
    const role = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.firstFormGroup.controls['name'].value,
      description: this.firstFormGroup.controls['description'].value,
      default_attributes: {},
      override_attributes: {},
      run_list: this.selectedItems,
      env_run_lists: [
    
      ]
    };
    this.store.dispatch(new CreateRole({server_id: this.serverId, org_id: this.orgId, role: role}));
  }

  createRoleThird() {
    

    this.creating = true;
    const role = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.firstFormGroup.controls['name'].value,
      description: this.firstFormGroup.controls['description'].value,
      default_attributes: new RoleAttributes(this.thirdFormGroup.controls['dattr'].value),
      override_attributes: {},
      run_list: this.selectedItems,
      env_run_lists: [
    
      ]
    };
    this.store.dispatch(new CreateRole({server_id: this.serverId, org_id: this.orgId, role: role}));
  }

  createRoleFourth() {
    this.creating = true;
    const role = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.firstFormGroup.controls['name'].value,
      description: this.firstFormGroup.controls['description'].value,
      default_attributes: new RoleAttributes(this.thirdFormGroup.controls['dattr'].value),
      override_attributes: new RoleAttributes(this.fourthFormGroup.controls['oattr'].value),
      run_list: this.selectedItems,
      env_run_lists: [
    
      ]
    };
    this.store.dispatch(new CreateRole({server_id: this.serverId, org_id: this.orgId, role: role}));
  }

  private resetCreateModal(): void {
    this.hookStatus = UrlTestState.Inactive;
    this.creating = false;
    this.firstFormGroup.reset();
    this.secondFormGroup.reset();
    this.thirdFormGroup.reset();
    this.fourthFormGroup.reset();

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
    let newValue = event.target.value;
    this.textareaID = event.target.id;
    try {
        // parse it to json
        this.data = JSON.parse(newValue);
        this.textareaID == 'dattr' ? this.dattrParseError = false : '';
        this.textareaID == 'oattr' ? this.oattrParseError = false : '';
    } catch (ex) {
        // set parse error if it fails
        this.textareaID == 'dattr' ? this.dattrParseError = true : '';
        this.textareaID == 'oattr' ? this.oattrParseError = true : '';
    }
    // update the form
    this.propagateChange(this.data);
  }

  // the method set in registerOnChange to emit changes back to the form
  private propagateChange = (_: any) => { };

}
