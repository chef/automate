import { Component, Input, Output, SimpleChanges, OnChanges, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'app-enable-content-modal',
  templateUrl: './enable-content-modal.component.html',
  styleUrls: ['./enable-content-modal.component.scss']
})
export class EnableContentModalComponent implements OnChanges {
  @Input() show: boolean;
  @Output() onTokenSubmit: EventEmitter<string> = new EventEmitter();

  public modalVisible = true;
  public modalLocked = true;
  public tokenForm: FormGroup;

  constructor(
    public fb: FormBuilder
  ) {
    this.tokenForm = fb.group({
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
    const formValues = this.tokenForm.value;
    const token = formValues.token.trim();

    this.onTokenSubmit.emit(token);
  }
}
