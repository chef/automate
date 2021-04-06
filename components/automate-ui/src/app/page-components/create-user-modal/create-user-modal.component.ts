import { Component, OnInit, Input, EventEmitter, OnDestroy } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { Regex } from 'app/helpers/auth/regex';
import { UsernameMapper } from 'app/helpers/auth/username-mapper';
import { ChefValidators } from 'app/helpers/auth/validator';
import { EntityStatus } from 'app/entities/entities';
import { Utilities } from 'app/helpers/utilities/utilities';
import { CreateUserPayload, CreateUser } from 'app/entities/users/user.actions';
import { createStatus, createError } from 'app/entities/users/user.selectors';

// pattern for valid usernames
const USERNAME_PATTERN = '[0-9A-Za-z_@.+-]+';

@Component({
  selector: 'app-create-user-modal',
  templateUrl: './create-user-modal.component.html',
  styleUrls: ['./create-user-modal.component.scss']
})
export class CreateUserModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<boolean>;

  public createUserForm: FormGroup;
  public conflictError = false;
  public passwordError = false;
  public modifyUsername = false; // Whether the edit Username form is open or not.
  public creatingUser = false;
  public visible = false;

  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    this.createUserForm = fb.group({
      // Must stay in sync with error checks in create-user-modal.component.html
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

  ngOnInit(): void {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.creatingUser = false;
        this.conflictError = false;
        this.passwordError = false;
        this.createUserForm.reset();
        this.visible = true;
        this.modifyUsername = false;
      });

    this.store.select(createStatus).pipe(
      filter(state => state === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => this.closeCreateModal());

    combineLatest([
      this.store.select(createStatus),
      this.store.select(createError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        this.creatingUser = false;
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictError = true;
          // Open the ID input so user can resolve it.
          this.modifyUsername = true;
        } else if (error.status === HttpStatus.BAD_REQUEST) {
          this.passwordError = true;
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

  closeCreateModal(): void {
    this.visible = false;
  }

  createUser(): void {
    this.creatingUser = true;
    const formValues = this.createUserForm.value;

    const userCreateReq: CreateUserPayload = {
      name: formValues.displayName.trim(),
      id: formValues.username,
      password: formValues.password
    };

    this.store.dispatch(new CreateUser(userCreateReq));
  }

  handleUsernameInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyUsername && !Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.createUserForm.controls.username.setValue(
        UsernameMapper.transform(this.createUserForm.controls.displayName.value.trim()));
    }
  }

  handlePasswordInput(event: KeyboardEvent): void {
    // This handles the case where a user first enters the password and confirmPassword
    // then goes back and changes the original password.
    // Since the match validator is only activated by changes to confirmPassword,
    // we have to manually revalidate confirmPassword here
    this.createUserForm.get('confirmPassword').updateValueAndValidity();
    if (!Utilities.isNavigationKey(event)) {
      this.passwordError = false;
    }
  }

  handleClose(): void {
    this.modifyUsername = false;
  }

  hasAttemptedInput(field: string): boolean {
    return (this.createUserForm.get(field).touched &&
      this.createUserForm.get(field).dirty);
  }

  hasAttemptedInputWithRequiredError(field: string): boolean {
    return (this.createUserForm.get(field).hasError('required') &&
      (this.createUserForm.get(field).touched || this.createUserForm.get(field).dirty));
  }

}
