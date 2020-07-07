import { Component, Input, SimpleChanges, OnChanges } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'app-register-modal',
  templateUrl: './register-modal.component.html',
  styleUrls: ['./register-modal.component.scss']
})
export class RegisterModalComponent implements OnChanges {
  @Input() show: boolean;

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

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['show']) {
      this.modalVisible = changes.show.currentValue;
    }
  }

  public closeModal(): void {
    this.modalVisible = false;
  }

  public onFormSubmit(): void {
    console.info('submitting!!!!');
  }
}
