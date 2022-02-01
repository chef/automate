import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'app-migration-slider',
  templateUrl: './migration-slider.component.html',
  styleUrls: ['./migration-slider.component.scss']
})
export class MigrationSliderComponent {
  public migrating = false;
  @HostBinding('class.active') isSlideOpen1 = false;

  constructor() { }

  closeMigrationSlider() {
    this.toggleSlide();
  }

  toggleSlide() {
    this.isSlideOpen1 = !this.isSlideOpen1;
  }

  migrationFile() {
    this.toggleSlide();
  }

  slidePanel() {
    console.log('migration part');
    this.isSlideOpen1 = true;
  }
}
