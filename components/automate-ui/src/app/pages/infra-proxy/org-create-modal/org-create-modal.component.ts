import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-org-create-modal',
  templateUrl: './org-create-modal.component.html',
  styleUrls: ['./org-create-modal.component.scss']
})
export class OrgCreateModalComponent implements OnInit {
  // @Input() type: string;
  @Input() visible = false;
  @Input() creating = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();
  @Input() createForm: FormGroup;

  public conflictError = false;

  constructor(

  ) { }

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  closeEvent(): void {
    this.close.emit();
  }

  createServerOrg(): void {
    this.createClicked.emit();
  }
}
