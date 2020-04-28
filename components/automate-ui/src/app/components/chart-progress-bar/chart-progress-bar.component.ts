import { Component, Input, OnChanges } from '@angular/core';
import { Styles } from './chart-progress-bar.model';

let nextId = 0;

@Component({
  selector: 'app-chart-progress-bar',
  templateUrl: './chart-progress-bar.component.html',
  styleUrls: [ './chart-progress-bar.component.scss' ]
})
export class ChartProgressBarComponent implements OnChanges {
  @Input() id = `progress-bar-${nextId++}`;
  @Input() size = 'large'; // large or small
  @Input() value: number;
  @Input() max: number;
  @Input() threshold: number[] = []; // [min, max]
  @Input() valueTooltip: string;
  @Input() maxTooltip: string;

  public styles: Styles = {
    bar: {},
    progress: {},
    threshold: {}
  };

  ngOnChanges() {
    this.populateStackedBar();
  }

  public populateStackedBar() {
    this.populateValueNow();
    this.populateThreshold();
  }

  public populateValueNow() {
    const value = this.getValuePercentage(this.value);
    this.styles.bar.width = (100 - value) + '%';
    this.styles.progress.width = value + '%';
  }

  public populateThreshold() {
    this.threshold.forEach((t, i) => {
      this.styles.threshold[i] = {
        marginLeft: this.getValuePercentage(t) + '%'
      };
    });
  }

  public getValuePercentage(value: number): number {
    return (value / (this.max || 100)) * 100;
  }

  get progressClassName(): string {
    const value = this.getValuePercentage(this.value);
    if (value < 25) { return 'low'; }
    if (value >= 25 && value < 50) { return 'mid'; }
    if (value >= 50 && value < 75) { return 'high'; }
  }
}
