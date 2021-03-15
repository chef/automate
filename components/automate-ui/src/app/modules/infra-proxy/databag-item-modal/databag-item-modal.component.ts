
import {
  Component,
  EventEmitter,
  Input,
  // OnDestroy,
  OnInit
} from '@angular/core';
import { combineLatest, Subject } from 'rxjs';
import { Store, select } from '@ngrx/store';
import { FormBuilder,  Validators, FormGroup } from '@angular/forms';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, pending } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import {
  saveStatus,
  saveError
} from 'app/entities/data-bags/data-bags.selectors';
import { DataBag } from 'app/entities/data-bags/data-bags.model';

@Component({
  selector: 'app-databag-item-modal',
  templateUrl: './databag-item-modal.component.html',
  styleUrls: ['./databag-item-modal.component.scss']
})
export class DatabagItemModalComponent implements OnInit {
  @Input() openEvent: EventEmitter<void>;
  @Input() server_Id: string;
  @Input() org_Id: string;

  public visible = false;
  public creating = false;
  public sending = false;
  public close = new EventEmitter();
  public createForm: FormGroup;
  public dataBag: DataBag;
  public conflictError = false;
  public itemAttrParseError = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder
  ) {
    this.createForm = this.fb.group({
      // Must stay in sync with error checks in create-notification-modal.component.html
      itemId: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      itemAttr:['{}']
    });
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.conflictError = false;
        this.visible = true;
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

  onChangeJSON(event: { target: { value: string } } ) {
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

  createDataBagItem(): void {
    this.creating = true;

    // const dataBagItem = {
    //   server_id: this.server_Id,
    //   org_id: this.org_Id,
    //   name: this.createForm.controls['itemId'].value.trim()
    //   // attr: this.createForm.controls['itemAttr'].value.trim()
    // };

    // this.store.dispatch(new CreateDataBagItem({dataBag: dataBag}));
  }

  private resetCreateModal(): void {
    this.creating = false;
    this.createForm.reset();
    this.conflictError = false;
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

