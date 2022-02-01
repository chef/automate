import {
  Directive,
  Output,
  EventEmitter,
  HostBinding,
  HostListener
} from '@angular/core';

@Directive({
  selector: '[appDragDrop]'
})
export class DragDropDirective {
  @HostBinding('class.fileover') fileOver: boolean;
  @Output() fileDropped = new EventEmitter<any>();

  // Dragover listener
  @HostListener('dragover', ['$event']) onDragOver(
    evt: { preventDefault: () => void;
    stopPropagation: () => void; }) {
      evt.preventDefault();
      evt.stopPropagation();
      this.fileOver = true;
    }

  // Dragleave listener
  @HostListener('dragleave', ['$event']) public onDragLeave(
    evt: { preventDefault: () => void;
    stopPropagation: () => void; }) {
      evt.preventDefault();
      evt.stopPropagation();
      this.fileOver = false;
    }

  // Drop listener
  @HostListener('drop', ['$event']) public ondrop(
    evt: { preventDefault: () => void;
    stopPropagation: () => void;
    dataTransfer: { files: any; }; }) {
      evt.preventDefault();
      evt.stopPropagation();
      this.fileOver = false;
      const files = evt.dataTransfer.files;
      if (files.length > 0) {
        this.fileDropped.emit(files);
      }
    }
}
