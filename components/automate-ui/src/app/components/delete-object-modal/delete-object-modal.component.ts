import { Component, EventEmitter, Input, Output } from '@angular/core';
import { User } from '../../entities/users/user.model';
import { ContinuousPipe } from '../../pipes/continuous.pipe';

@Component({
  standalone: false,
  selector: 'app-delete-object-modal',
  templateUrl: './delete-object-modal.component.html',
  styleUrls: ['./delete-object-modal.component.scss']
})
export class DeleteObjectModalComponent extends ContinuousPipe {
  @Input() visible = false;
  @Input() objectNoun: string;
  @Input() objectName: string;
  @Input() moreDetails: string; // additional details after "action cannot be undone"
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
