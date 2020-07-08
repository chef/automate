import { Component, Input, Output, SimpleChanges, OnChanges, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';

import {
  Credentials
} from 'app/entities/cds/cds.model';

@Component({
  selector: 'app-enable-content-modal',
  templateUrl: './enable-content-modal.component.html',
  styleUrls: ['./enable-content-modal.component.scss']
})
export class EnableContentModalComponent implements OnChanges {
  @Input() show: boolean;
  @Output() onCredentialsSubmit: EventEmitter<Credentials> = new EventEmitter();

  public modalVisible = true;
  public modalLocked = false;
  public credentialsForm: FormGroup;

  constructor(
    public fb: FormBuilder,
    private router: Router
  ) {
    this.credentialsForm = fb.group({
      clientId: ['', [Validators.required]],
      clientSecret: ['', [Validators.required]],
      tenantSpecificUrl: ['', [Validators.required]]
    });
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['show']) {
      this.modalVisible = changes.show.currentValue;
    }
  }

  public closeModal(): void {
    this.router.navigate(['/']);
  }

  public onFormSubmit(): void {
    const formValues = this.credentialsForm.value;

    const credentials: Credentials = {
      clientId: formValues.clientId.trim(),
      clientSecret: formValues.clientSecret.trim(),
      tenantSpecificUrl: formValues.tenantSpecificUrl.trim()
    };

    this.onCredentialsSubmit.emit(credentials);
  }
}
