import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

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
  @Input() createForm: FormGroup;

  public conflictError = false;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  // WIP for handle input
  // public handleNameInput(): void {
  // }

  // public handleDescriptionInput(): void {
  // }

  // public handleFqdnInput(): void {
  // }

  // public handleIpAddressInput(): void {
  // }

  closeEvent(): void {
    this.close.emit();
  }

  createChefServer(): void {
    this.createClicked.emit();
  }

}

