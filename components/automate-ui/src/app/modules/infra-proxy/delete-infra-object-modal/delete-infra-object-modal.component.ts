import { Component, EventEmitter, Input, Output } from '@angular/core';
import { User } from 'app/entities/users/user.model';
import { ContinuousPipe } from '../../../pipes/continuous.pipe';

@Component({
  selector: 'app-delete-infra-object-modal',
  templateUrl: './delete-infra-object-modal.component.html',
  styleUrls: ['./delete-infra-object-modal.component.scss']
})
export class DeleteInfraObjectModalComponent extends ContinuousPipe {
  @Input() visible = false;
  @Input() objectNoun: string;
  @Input() objectAction: string;
  @Input() custom = false; // use for custom markup and messaging

  @Output() close = new EventEmitter();
  @Output() deleteClicked = new EventEmitter<User>();

  closeEvent(): void {
    this.close.emit();
  }

  deleteObject(): void {
    this.deleteClicked.emit();
  }
}
