
import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit
} from '@angular/core';
import { combineLatest, Subject } from 'rxjs';
import { Store, select } from '@ngrx/store';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, pending } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import {
  saveStatus,
  saveError
} from 'app/entities/data-bags/data-bag-details.selector';
import { DataBagItem } from 'app/entities/data-bags/data-bags.model';
import {
  CreateDataBagItem,
  GetDataBagItems } from 'app/entities/data-bags/data-bag-details.actions';

@Component({
  selector: 'app-create-databag-item-modal',
  templateUrl: './create-databag-item-modal.component.html',
  styleUrls: ['./create-databag-item-modal.component.scss']
})
export class CreateDatabagItemModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<void>;
  @Input() server_Id: string;
  @Input() org_Id: string;
  @Input() name: string;
  @Input() currentPage: number;

  public visible = false;
  public creating = false;
  public sending = false;
  public close = new EventEmitter();
  public createForm: FormGroup;
  public dataBagItem: DataBagItem;
  public conflictError = false;
  public itemAttrParseError = false;
  public per_page = 9;
  public id: {};
  public attr: {};
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder
  ) {
    this.createForm = this.fb.group({
      itemId: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      itemAttr: ['{}']
    });
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.conflictError = false;
        this.visible = true;
      });

    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.visible && !pending(state)))
      .subscribe(state => {
        this.creating = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
        }
      });

    combineLatest([
      this.store.select(saveStatus),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.visible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictError = true;
        } else {
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
    const payload = {
      databagName: '',
      server_id: this.server_Id,
      org_id: this.org_Id,
      name: this.name,
      page: this.currentPage,
      per_page: this.per_page
    };
    this.store.dispatch(new GetDataBagItems(payload));
  }

  onChangeJSON(event: { target: { value: string } }) {
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
    if(this.createForm.controls['itemAttr'].value) {
      this.attr = JSON.parse(this.createForm.controls['itemAttr'].value.trim());
    }
    this.id = {'id': this.createForm.controls['itemId'].value.trim()};
    const dataBagItem = {
      server_id: this.server_Id,
      org_id: this.org_Id,
      name: this.name,
      data: {
        ...this.id,
        ...this.attr
      }
    };

    this.store.dispatch(new CreateDataBagItem({dataBagItem: dataBagItem}));
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

