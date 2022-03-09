import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
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
import {
  CreateDataBag
} from 'app/entities/data-bags/data-bags.actions';
import { Utilities } from 'app/helpers/utilities/utilities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-create-data-bag-modal',
  templateUrl: './create-data-bag-modal.component.html',
  styleUrls: ['./create-data-bag-modal.component.scss']
})
export class CreateDataBagModalComponent implements OnInit, OnDestroy {
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
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private telemetryService: TelemetryService
  ) {
    this.createForm = this.fb.group({
      // Must stay in sync with error checks in create-notification-modal.component.html
      name: ['', [Validators.required,
              Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]]
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
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  createDataBag(): void {
    this.creating = true;

    const dataBag = {
      server_id: this.server_Id,
      org_id: this.org_Id,
      name: this.createForm.controls['name'].value.trim()
    };

    this.store.dispatch(new CreateDataBag({dataBag: dataBag}));
    this.telemetryService.track('InfraServer_Databags_Create');
  }

  private resetCreateModal(): void {
    this.creating = false;
    this.createForm.reset();
    this.conflictError = false;
  }

}
