import { Component, Input, OnInit, OnChanges } from '@angular/core';
import { Styles } from './chart-progress-bar.model';

@Component({
  selector: 'app-chart-progress-bar',
  templateUrl: './chart-progress-bar.component.html',
  styleUrls: [ './chart-progress-bar.component.scss' ]
})

export class ChartProgressBarComponent implements OnInit, OnChanges {

  @Input() size = 'large'; // large or small
  @Input() value: number;
  @Input() min: number;
  @Input() max: number;
  @Input() threshold: number[]; // [min, max]
  @Input() valueTooltip: any;
  @Input() maxTooltip: any;

  public styles: Styles = {
    bar: {},
    container: {},
    progress: {},
    threshold: { point1: {}, point2: {}}
  };
  public tooltips = {
    value: '',
    max: ''
  };

  constructor() {}

  ngOnInit() {
   this.createStackedBar();
   this.populateStackedBar();
  }

  ngOnChanges() {
    this.populateStackedBar();
  }

  public createStackedBar() {
    this.size === 'large' ? this.createLargeBar() : this.createSmallBar();
  }

  public populateStackedBar() {
    this.populateValueNow();
    this.populateTooltips();
    this.populateThreshold();
    this.populateProgressBackground();
  }

  public createLargeBar() {
    this.styles.bar = { borderRadius: '8px', height: '16px' };
    this.styles.container.height = '16px';
    this.styles.progress.borderRadius = '20px';
    this.styles.progress.height = '16px';
    this.styles.threshold.height = '16px';
  }

  public createSmallBar() {
    this.styles.bar.background = 'none';
    this.styles.bar.boxShadow = 'none';
    this.styles.progress.borderRadius = '8px';
    this.styles.container.height = '8px';
    this.styles.progress.height = '8px';
    this.styles.threshold.height = '8px';
  }

  public populateValueNow() {
    const value = this.getValuePercentage(this.value);
    this.styles.bar.width = (100 - value) + '%';
    this.styles.progress.width = value + '%';

    // Have no progress bar right radius
    // until progress reaches 100%
    this.styles.progress.borderRadius =
      (this.size === 'large' && value < 99)
      ? '20px 0 0 20px'
      : '20px';

    // Have no bar left radius
    // when progress is greater than 1%
    this.styles.bar.borderRadius =
    (this.size === 'large' && value > 0)
    ? '0 20px 20px 0'
    : '20px';

  }

  public populateTooltips() {
    this.tooltips.value = this.valueTooltip;
    this.tooltips.max = this.maxTooltip;
  }

  public populateThreshold() {
    if (this.threshold && this.threshold[0] > 0) {
      this.styles.threshold.point1.height = '16px';
      this.styles.threshold.point1.marginLeft = this.getValuePercentage(this.threshold[0]) + '%';
    }
    if (this.threshold && this.threshold[1] > 0) {
      this.styles.threshold.point2.height = '16px';
      this.styles.threshold.point2.marginLeft = this.getValuePercentage(this.threshold[1]) + '%';
    }
  }

  // Increase the background color as progress value increases
  public populateProgressBackground() {
    const progressValue = this.getValuePercentage(this.value);
    let endColor = '#E71D36';

    if (progressValue <= 25) { endColor = '#f9891a'; }
    if (progressValue > 26 && progressValue <= 50) { endColor = '#f36628'; }
    if (progressValue > 51 && progressValue <= 75) { endColor = '#ee472f'; }

    this.styles.progress.background = `linear-gradient(90deg, #FEA900 0%, ${endColor})`;
  }

  getValuePercentage(value): number {
    return (value / (this.max || 100)) * 100;
  }

}
