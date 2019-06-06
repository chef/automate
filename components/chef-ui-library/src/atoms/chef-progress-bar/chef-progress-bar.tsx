import { Component, Prop } from '@stencil/core';

/**
 * @description
 * The `<chef-progress-bar>` atom is used to display progress status information.
 *
 * @example
 * <chef-progress-bar value="10"></chef-progress-bar>
 * <chef-progress-bar value="50"></chef-progress-bar>
 * <chef-progress-bar value="90"></chef-progress-bar>
 *
 * @example
 * <chef-progress-bar value="0.1" value-min="0" value-max="1"></chef-progress-bar>
 * <chef-progress-bar value="0.5" value-min="0" value-max="1"></chef-progress-bar>
 * <chef-progress-bar value="0.9" value-min="0" value-max="1"></chef-progress-bar>
 *
 * @example
 * <chef-progress-bar value="10" prefix-text="10%"></chef-progress-bar>
 * <chef-progress-bar value="50" prefix-text="50%"></chef-progress-bar>
 * <chef-progress-bar value="90" prefix-text="90%"></chef-progress-bar>
 *
 * @example
 * <chef-progress-bar value="10" suffix-text="10%"></chef-progress-bar>
 * <chef-progress-bar value="50" suffix-text="50%"></chef-progress-bar>
 * <chef-progress-bar value="90" suffix-text="90%"></chef-progress-bar>
 *
 * @example
 * <chef-progress-bar value="10" prefix-text="10% Complete" suffix-text="90% Remaining"></chef-progress-bar>
 * <chef-progress-bar value="50" prefix-text="50% Complete" suffix-text="50% Remaining"></chef-progress-bar>
 * <chef-progress-bar value="90" prefix-text="90% Complete" suffix-text="10% Remaining"></chef-progress-bar>
 *
 * @example
 * <chef-progress-bar value="10" prefix-text="10% complete" suffix-text="01:00:00 until finished"></chef-progress-bar>
 * <chef-progress-bar value="50" prefix-text="50% complete" suffix-text="00:30:00 until finished"></chef-progress-bar>
 * <chef-progress-bar value="90" prefix-text="90% complete" suffix-text="00:01:00 until finished"></chef-progress-bar>
 *
 * @example
 * <chef-progress-bar value="0" value-max="150" prefix-text="Upload starting" suffix-text="0 of 150 uploaded"></chef-progress-bar>
 * <chef-progress-bar value="75" value-max="150" prefix-text="Upload in progress" suffix-text="75 of 150 uploaded"></chef-progress-bar>
 * <chef-progress-bar value="150" value-max="150"  prefix-text="Upload completed" suffix-text="150 of 150 uploaded"></chef-progress-bar>
 */
@Component({
  tag: 'chef-progress-bar',
  styleUrl: 'chef-progress-bar.scss'
})
export class ChefProgressBar {
  /**
   * The current progress value.
   */
  @Prop() value = 0;

  /**
   * The minimum progress value.
   */
  @Prop() valueMin = 0;

  /**
   * The maximum progress value.
   */
  @Prop() valueMax = 100;

  /**
   * Optional text to display at the start of the progress bar.
   */
  @Prop() prefixText = '';

  /**
   * Optional text to display at the end of the progress bar.
   */
  @Prop() suffixText = '';

  get hasProgressText(): boolean {
    return this.prefixText.length > 0 || this.suffixText.length > 0;
  }

  get ariaValueText(): string {
    return this.hasProgressText ? [this.prefixText, this.suffixText].join(' ').trim() : null;
  }

  get ariaValueNow(): string {
    return `${this.value}`;
  }

  get ariaValueMin(): string {
    return `${this.valueMin}`;
  }

  get ariaValueMax(): string {
    return `${this.valueMax}`;
  }

  hostData() {
    return {
      'role': 'progressbar',
      'aria-valuenow': this.ariaValueNow,
      'aria-valuemin': this.ariaValueMin,
      'aria-valuemax': this.ariaValueMax,
      'aria-valuetext': this.ariaValueText
    };
  }

  render() {
    const progressText = this.hasProgressText ? (
      <div class="progress-text" aria-hidden>
        <div class="prefix">{ this.prefixText }</div>
        <div class="suffix">{ this.suffixText }</div>
      </div>
    ) : null;

    const valueStyle = { width: `${(this.value / (this.valueMax - this.valueMin)) * 100}%` };
    const progressBars = (
      <div class="progress-bars" aria-hidden>
        <div class="bar total"></div>
        <div class="bar value" style={valueStyle}></div>
      </div>
    );

    return [progressText, progressBars];
  }
}
