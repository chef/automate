import {
  Component,
  Input,
  Output,
  OnInit,
  OnChanges,
  ChangeDetectionStrategy,
  Renderer2,
  ElementRef,
  HostBinding,
  HostListener,
  EventEmitter
} from '@angular/core';
import { RollupState } from '../../types/types';

@Component({
  selector: 'app-node-rollup',
  templateUrl: './node-rollup.component.html',
  styleUrls: ['./node-rollup.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})


export class NodeRollupComponent implements OnInit, OnChanges {
  // 'RollupState | RollupState' syntax is a workaround for an issue
  // with webpack type exports/imports on Inputs
  @Input() name: RollupState | RollupState;
  @Input() active: false;
  @Input() count: string;
  @Output() activated: EventEmitter<any> = new EventEmitter();
  @HostBinding('attr.role') role = 'button';
  @HostBinding('attr.tabindex') tabindex = '0';

  @HostListener('click', ['$event']) onClick() {
    this.activated.emit(null);
  }

  @HostListener('keyup.enter', ['$event']) onKeyEnter() {
    this.activated.emit(null);
  }

  @HostListener('keyup.space', ['$event']) onKeySpace() {
    this.activated.emit(null);
  }

  constructor(
    private el: ElementRef,
    private renderer: Renderer2
  ) {}

  ngOnInit() {
    this.renderer.addClass(this.el.nativeElement, this.name);
  }

  ngOnChanges() {
    if (this.active) {
      this.renderer.addClass(this.el.nativeElement, 'active');
    } else {
      this.renderer.removeClass(this.el.nativeElement, 'active');
    }
  }

  label(name: string): string {
    switch (name) {
      case 'success':
        return 'Successful Nodes';
      case 'failure':
        return 'Failed Nodes';
      case 'missing':
        return 'Missing Nodes';
      case 'compliant':
        return 'Compliant Nodes';
      case 'uncompliant':
        return 'Uncompliant Nodes';
      case 'skipped':
        return 'Skipped Nodes';
      default:
        return 'Total Nodes';
    }
  }
}
