import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';

@Component({
  selector: 'app-org-create-edit-modal',
  templateUrl: './org-create-edit-modal.component.html',
  styleUrls: ['./org-create-edit-modal.component.scss']
})
export class OrgCreateEditModalComponent implements OnInit {
  @Input() visible = false;
  @Input() type = 'create';
  @Output() close = new EventEmitter();

  public orgForm: FormGroup;

  constructor(
    private fb: FormBuilder

  ) { }

  ngOnInit() {
    this.orgForm = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      orgId: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
  }

  closeEvent(): void {
    this.close.emit();
  }
}
