import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';

@Component({
  selector: 'app-create-chef-server-modal',
  templateUrl: './create-chef-server-modal.component.html',
  styleUrls: ['./create-chef-server-modal.component.scss']
})
export class CreateChefServerModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();

  public createForm: FormGroup;

  constructor(
    private fb: FormBuilder

  ) { }

  ngOnInit() {
    this.createForm = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      server_id: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      fqdn: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]      
    });
  }

  public handleNameInput(): void {
  }

  public handleServerIdInput(): void {
  }

  public handleFqdnInput(): void {
  }

  closeEvent(): void {
    this.close.emit();
  }

  createChefServer(): void {
  }

  createObject(): void {
  }

}

