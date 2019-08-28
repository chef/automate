import { Component, EventEmitter, Input, Output } from '@angular/core';
import { User } from 'app/entities/users/user.model';

@Component({
  selector: 'app-delete-object-modal',
  templateUrl: './delete-object-modal.component.html',
  styleUrls: ['./delete-object-modal.component.scss']
})
export class DeleteObjectModalComponent {
  @Input() visible = false;
  @Input() objectNoun: string;
  @Input() objectName: string;
  @Input() moreDetails: string; // additional details after "action cannot be undone"
  @Input() nodeCredObject: string;

  @Output() close = new EventEmitter();
  @Output() deleteClicked = new EventEmitter<User>();

  closeEvent(): void {
    this.close.emit();
  }

  deleteObject(): void {
    this.deleteClicked.emit();
  }
}
