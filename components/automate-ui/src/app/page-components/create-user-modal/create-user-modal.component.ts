import { Component, OnInit, Input, EventEmitter, OnChanges, SimpleChanges, OnDestroy } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Subject } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { UsernameMapper } from 'app/helpers/auth/username-mapper';
import { ChefValidators } from 'app/helpers/auth/validator';
import { EntityStatus } from 'app/entities/entities';
import { CreateUserPayload, CreateUser } from 'app/entities/users/user.actions';
import { createStatus } from 'app/entities/users/user.selectors';

// pattern for valid usernames
const USERNAME_PATTERN = '[0-9A-Za-z_@.+-]+';

@Component({
  selector: 'app-create-user-modal',
  templateUrl: './create-user-modal.component.html',
  styleUrls: ['./create-user-modal.component.scss']
})
export class CreateUserModalComponent implements OnInit, OnDestroy, OnChanges {
  @Input() openEvent: EventEmitter<boolean>;

  public createUserForm: FormGroup;
  public conflictErrorEvent: EventEmitter<boolean>;
  public passwordErrorEvent: EventEmitter<boolean>;

  public conflictError = false;
  public passwordError = false;
  public modifyUsername = false; // Whether the edit Username form is open or not.
  public creatingUser = false;
  public visible = false;

  private isDestroyed = new Subject<boolean>();

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.visible && (changes.visible.currentValue as boolean)) {
      // TODO
    }
  }

  closeCreateModal(): void {
    this.visible = false;
  }

  //////////////////////////////////// FROM user-management.component.ts

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    this.createUserForm = fb.group({
      // Must stay in sync with error checks in user-form.component.html
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: ['', [Validators.required, Validators.pattern(USERNAME_PATTERN)]],
      // length validator must be consistent with
      // backend password rules in local-user-service/password/password.go
      password: ['', [
        Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['', [Validators.required, ChefValidators.matchFieldValidator('password')]]
    });
  }

  public createUser(): void {
    this.creatingUser = true;
    const formValues = this.createUserForm.value;

    const userCreateReq = <CreateUserPayload>{
      name: formValues.displayName.trim(),
      id: formValues.username,
      password: formValues.password
    };

    this.store.dispatch(new CreateUser(userCreateReq));
  }

  //////////////////////////////////// FROM user-form.component.ts

  ngOnInit() {
    // TODO
    // this.conflictErrorEvent.pipe(takeUntil(this.isDestroyed))
    //   .subscribe((isConflict: boolean) => {
    //     this.conflictError = isConflict;
    //     // Open the ID input on conflict so user can resolve it.
    //     this.modifyUsername = isConflict;
    //   });

    // this.passwordErrorEvent.pipe(takeUntil(this.isDestroyed))
    //   .subscribe((badPassword: boolean) => {
    //     this.passwordError = badPassword;
    //     this.createUserForm.get('password').reset();
    //     this.createUserForm.get('confirmPassword').reset();
    //   });

    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.creatingUser = false;
        this.visible = true;
      });

    this.store.select(createStatus).pipe(
      filter(state => state === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => this.closeCreateModal());
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  handleUsernameInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyUsername && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createUserForm.controls.username.setValue(
        UsernameMapper.transform(this.createUserForm.controls.displayName.value.trim()));
    }
  }

  handleClose(): void {
    this.modifyUsername = false;
  }

  hasAttemptedInput(field: string): boolean {
    return (this.createUserForm.get(field).touched &&
      this.createUserForm.get(field).dirty);
  }

  leftInputEmpty(field: string): boolean {
    return ((this.createUserForm.get(field).hasError('required') &&
      this.createUserForm.get(field).touched) ||
      (this.createUserForm.get(field).hasError('pattern') &&
        this.createUserForm.get(field).dirty));
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
