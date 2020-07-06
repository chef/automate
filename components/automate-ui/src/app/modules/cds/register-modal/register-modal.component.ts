import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'app-register-modal',
  templateUrl: './register-modal.component.html',
  styleUrls: ['./register-modal.component.scss']
})
export class RegisterModalComponent {
  public modalVisible = true;
  public modalLocked = true;
  public tokenform: FormGroup;

  constructor(
    public fb: FormBuilder
  ) {
    this.tokenform = fb.group({
      token: ['', [Validators.required]]
    });
  }

  public closeModal(): void {
    this.modalVisible = false;
  }

  public onFormSubmit(): void {
    console.info('submitting!!!!');
  }
}
