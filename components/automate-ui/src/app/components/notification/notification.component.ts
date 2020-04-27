import { Component, Input, HostBinding, Output, EventEmitter, AfterViewInit } from '@angular/core';
import { trigger, transition, style, animate } from '@angular/animations';
import { Type } from 'app/entities/notifications/notification.model';


const inOutAnim = trigger('inOutAnim', [
  transition(':enter', [
    style({ opacity: 0 }),
    animate('220ms ease-out', style({ opacity: 1 }))
  ]),
  transition(':leave', [
    style({ opacity: 1 }),
    animate('195ms ease-in', style({ opacity: 0 }))
  ])
]);

const slideAnim = trigger('slideAnim', [
  transition(':enter', [
    style({
      opacity: 0,
      transform: 'scale3d(0,1,1)',
      transformOrigin: 'left'
    }),
    animate('300ms 100ms ease-out',
      style({
        opacity: 1,
        transform: 'scale3d(1,1,1)',
        transformOrigin: 'left'
      }))
  ]),
  transition(':leave', [
    style({ opacity: 1, transformOrigin: 'right' }),
    animate('195ms ease-in', style({ opacity: 0, transform: 'scale3d(0,1,1)' }))
  ])
]);

@Component({
  selector: 'app-chef-notification',
  templateUrl: './notification.component.html',
  styleUrls: ['./notification.component.scss'],
  animations: [inOutAnim, slideAnim]
})
export class ChefNotificationComponent implements AfterViewInit {

  @Input() type: Type;
  @Input() timeout = 5;
  @Output() dismissed = new EventEmitter();

  // @HostBinding('@inOutAnim') inOutAnim;
  @HostBinding('@slideAnim') slideAnim;
  @HostBinding('class') get theType() {return this.type; }

  timeOutRef;

  constructor() {}

  ngAfterViewInit() {
    this.timeOutRef = setTimeout(this.handleClose, this.timeout * 1000);
  }

  handleClose = () => {
    clearTimeout(this.timeOutRef);
    this.dismissed.emit();
  }
}
