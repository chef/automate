import { Component, Input, OnInit, EventEmitter } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit {
  @Input() createUserForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Input() passwordErrorEvent: EventEmitter<boolean>;

  public conflictError = false;
  public passwordError = false;

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
    // TODO add takeUntil(this.isDestroyed) to unsubscribe

    this.passwordErrorEvent.subscribe((badPassword: boolean) => {
      this.passwordError = badPassword;
      this.createUserForm.get('password').reset();
      this.createUserForm.get('confirmPassword').reset();
    });
    // TODO add takeUntil(this.isDestroyed) to unsubscribe
  }

  handleUsernameInput(event: KeyboardEvent): void {
    // no change if navigation key entered
    if (event.key === 'Shift' || event.key === 'Tab') {
      return;
    }
    this.conflictError = false;
  }
}
