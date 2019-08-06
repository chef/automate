import { Component, Element, Listen, Method, Prop, State, h } from '@stencil/core';
import { Dictionary } from 'lodash';
import zip from 'lodash/fp/zip';
import multiply from 'lodash/fp/multiply';
import groupBy from 'lodash/fp/groupBy';
import map from 'lodash/fp/map';
import { arc, pie } from 'd3-shape';

const RADIUS = 100;
const PADDING = 20;

let UID = 0;

/**
 * @description
 * chef-radial-chart is used to create radial charts. Data points are specified using
 * chef-data-point. Colors and various styling can be done through css by setting a
 * class on chef-data-point. Secondary data points are displayed as an outer ring
 * around the primary chart. There is also a slot, innerText, provided to add text
 * to the center of the radial.
 * Modifying data points after render will not automatically trigger a re-render. After
 * modifying your data points call the `updateDataPoints()` method on the component to
 * trigger a re-render.
 *
 * @example
 * <style>
 *   chef-radial-chart .failed, chef-radial-chart .critical {
 *     color: var(--chef-critical);
 *   }
 *   chef-radial-chart .warning, chef-radial-chart .warning {
 *     color: var(--chef-warning);
 *   }
 *   chef-radial-chart .success, chef-radial-chart .major {
 *     color: var(--chef-ok);
 *   }
 *   chef-radial-chart .skipped, chef-radial-chart .minor {
 *     color: var(--chef-unknown);
 *   }
 * </style>
 * <chef-radial-chart style="width: 220px; height: 220px;">
 *   <span slot="innerText">Text for the center of the chart</span>
 *
 *   <chef-data-point value="4" class="failed">4 Failed</chef-data-point>
 *   <chef-data-point value="3" class="warning">3 Warning</chef-data-point>
 *   <chef-data-point value="2" class="success">2 Successful</chef-data-point>
 *   <chef-data-point value="1" class="skipped">1 Skipped</chef-data-point>
 *
 *   <chef-data-point value="4" secondary class="critical">Critical</chef-data-point>
 *   <chef-data-point value="3" secondary class="warning">Warning</chef-data-point>
 *   <chef-data-point value="2" secondary class="major">Major</chef-data-point>
 *   <chef-data-point value="1" secondary class="minor">Minor</chef-data-point>
 * </chef-radial-chart>
 *
 * @example
 * <style>
 *   chef-radial-chart .failed, chef-radial-chart .critical {
 *     color: var(--chef-critical);
 *   }
 *   chef-radial-chart .warning, chef-radial-chart .warning {
 *     color: var(--chef-warning);
 *   }
 *   chef-radial-chart .success, chef-radial-chart .major {
 *     color: var(--chef-ok);
 *   }
 *   chef-radial-chart .skipped, chef-radial-chart .minor {
 *     color: var(--chef-unknown);
 *   }
 * </style>
 * <chef-radial-chart gap-size="5" style="width: 220px; height: 220px;">
 *   <span slot="innerText">Text for the center of the chart</span>
 *
 *   <chef-data-point value="" class="failed">4 Failed</chef-data-point>
 *   <chef-data-point value="" class="warning">3 Warning</chef-data-point>
 *   <chef-data-point value="2" class="success">2 Successful</chef-data-point>
 *   <chef-data-point value="1" class="skipped">1 Skipped</chef-data-point>
 * </chef-radial-chart>
 */
@Component({
  tag: 'chef-radial-chart',
  styleUrl: './chef-radial-chart.scss'
})
export class ChefRadialChart {

  /**
   * Optionally set an id. This is automatically set to a unique ID if left blank.
   */
  @Prop() id: string;

  /**
   * The width of the gap (in degrees) to apply between chart segments.
   */
  @Prop({ reflectToAttr: true }) gapSize = 2;

  /**
   * Optionally hide tooltips. They are shown by default.
   */
  @Prop() tooltips = true;

  @State() dataPoints: Dictionary<HTMLChefDataPointElement[]> = { primary: [], secondary: [] };

  @Element() el: HTMLElement;

  @Listen('updated') handleDataPointUpdated() {
    this.updateDataPoints();
  }

  constructor() {
    this.id = this.id || `radial${UID++}`;
  }

  @Method()
  async updateDataPoints() {
    const dataPoints = Array.from(this.el.querySelectorAll('chef-data-point'));
    const dataPointsWithValues = [];
    dataPoints.forEach((d) => (Number(d.value) > 0) ? dataPointsWithValues.push(d) : null);
    this.dataPoints = groupBy((d) => d.secondary ? 'secondary' : 'primary', dataPointsWithValues);
  }

  componentWillLoad() {
    this.updateDataPoints();
  }

  render() {
    const pieChart = pie().sort(null).value((d) => d.value).padAngle(this.gapSize * Math.PI / 180);
    const { primary, secondary } = this.dataPoints;
    const hasSecondaryPoints = this.dataPoints.secondary !== undefined;
    const primarySegments = zip(pieChart(primary), primary);
    const secondarySegments = hasSecondaryPoints ? zip(pieChart(secondary), secondary) : null;
    const viewBox = [ -1, -1, 2, 2 ].map(multiply(RADIUS + PADDING));

    const primaryConfig = {
      innerRadius: RADIUS - 18,
      outerRadius: RADIUS
    };

    const secondaryConfig = {
      innerRadius: RADIUS + 10,
      outerRadius: RADIUS + 15,
      cornerRadius: 5
    };

    return [
      <div class="innerText">
        <slot name="innerText" />
      </div>,
      <svg width="100%" height="100%" viewBox={ viewBox.join(' ') }>
        <g class="primary">
          { map(this.drawSegment.bind(this, primaryConfig), primarySegments) }
        </g>
        <g class="secondary">
          { map(this.drawSegment.bind(this, secondaryConfig), secondarySegments) }
        </g>
      </svg>,
      map(this.createTooltips.bind(this), primarySegments),
      <slot />
    ];
  }

  createTooltips([segment, element]) {
    if (this.tooltips === true) {
      return (
        <chef-tooltip for={ `${this.id}${segment.index}` } follow innerHTML={ element.innerHTML }></chef-tooltip>
      );
    } else {
      return;
    }
  }

  drawSegment({ innerRadius, outerRadius, cornerRadius = 0 }, [segment, element]) {
    const path = arc().innerRadius(innerRadius)
                      .outerRadius(outerRadius)
                      .cornerRadius(cornerRadius)(segment);
    return (
      <path
        class={ `${element.className} segment` }
        id={ `${this.id}${segment.index}` }
        d={ path }
        onMouseOver={ this.handleHover.bind(this) }
      />
    );
  }

  handleHover(event) {
    const parent = event.target.parentNode;
    const segment = parent.removeChild(event.target);
    parent.appendChild(segment);
  }

}
