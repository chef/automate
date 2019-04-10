import { Component } from '@stencil/core';

/**
 * @description
 * Chef colors
 */

@Component({
  tag: 'chef-colors',
  styleUrl: 'chef-colors.scss'
})
export class ChefColors {

  render() {
    return (
      <div>
        <h2>Color Guidelines</h2>
        <p>The Automate UI is built from a core set of primary, secondary, and greyscale colors. Secondary colors and additional shades are used to convey status and are used in backgrounds.</p>

        <h3>Color Palettes</h3>
        <div class="chef-flex-wrapper">
          <h4>Primary</h4>
          <p>Used for brand accents and actionable elements such as links and buttons.</p>
          <div class="chef-flex-row">
            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-primary"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Primary</strong></h4>
                <h6>--chef-primary</h6>
                <ul>
                  <li>HSL 23 100% 50%</li>
                  <li>RGB 254, 97, 0</li>
                  <li>HEX #FE6100</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-primary-dark"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Primary Dark</strong></h4>
                <h6>--chef-primary-dark</h6>
                <ul>
                  <li>HSL 234, 22%, 17%</li>
                  <li>RGB 34, 36, 53</li>
                  <li>HEX #222435</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-primary-bright"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Primary Bright</strong></h4>
                <h6>--chef-primary-bright</h6>
                <ul>
                  <li>HSL 226, 88%, 58%</li>
                  <li>RGB 56, 100, 242</li>
                  <li>HEX #3864F2</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-primary-light"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Primary Light</strong></h4>
                <h6>--chef-primary-light</h6>
                <ul>
                  <li>HSL 223, 100%, 70%</li>
                  <li>RGB 100, 143, 255</li>
                  <li>HEX #648FFF</li>
                </ul>
              </div>
            </div>
          </div>

          <h4>Secondary</h4>
          <p>Used to communicate status resulting from Chef client runs and compliance scans.</p>
          <div class="chef-flex-row">
            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-critical"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Critical</strong></h4>
                <h6>--chef-critical</h6>
                <ul>
                  <li>HSL 331, 72%, 51%</li>
                  <li>RGB 220, 38, 127</li>
                  <li>HEX #DC267F</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-major"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Major</strong></h4>
                <h6>--chef-major</h6>
                <ul>
                  <li>HSL 293, 67%, 51%</li>
                  <li>RGB 194, 45, 213</li>
                  <li>HEX #C22DD5</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-minor"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Minor</strong></h4>
                <h6>--chef-minor</h6>
                <ul>
                  <li>HSL 251, 83%, 65%</li>
                  <li>RGB 120, 94, 240</li>
                  <li>HEX #785EF0</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-3 chef-card-item">
              <div class="chef-color-swatch chef-success"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Success</strong></h4>
                <h6>--chef-success</h6>
                <ul>
                  <li>HSL 208, 100%, 43%</li>
                  <li>RGB 0, 117, 219</li>
                  <li>HEX #0075DB</li>
                </ul>
              </div>
            </div>
          </div>

          <h4>Greyscale</h4>
          <p>Used for background color fills and disabled states.</p>
          <div class="chef-flex-row">
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-dark-grey"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Dark Grey</strong></h4>
                <h6>--chef-dark-grey</h6>
                <ul>
                  <li>HSL 180, 4%, 45%</li>
                  <li>RGB 111, 120, 120</li>
                  <li>HEX #6F7878</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-grey"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Grey</strong></h4>
                <h6>--chef-grey</h6>
                <ul>
                  <li>HSL 200, 10%, 89%</li>
                  <li>RGB 223, 227, 229</li>
                  <li>HEX #DFE3E5</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-light-grey"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Light Grey</strong></h4>
                <h6>--chef-light-grey</h6>
                <ul>
                  <li>HSL 203, 19%, 92%</li>
                  <li>RGB 230, 235, 238</li>
                  <li>HEX #E6EBEE</li>
                </ul>
              </div>
            </div>

            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-lightest-grey"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Lightest Grey</strong></h4>
                <h6>--chef-lightest-grey</h6>
                <ul>
                  <li>HSL 204, 26%, 96%</li>
                  <li>RGB 243, 246, 248</li>
                  <li>HEX #F3F6F8</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-white"></div>
              <div class="chef-card-info">
                <h4><strong>Chef White</strong></h4>
                <h6>--chef-white</h6>
                <ul>
                  <li>HSL 0, 0%, 100%</li>
                  <li>RGB 255, 255, 255</li>
                  <li>HEX #FFFFFF</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-black"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Black</strong></h4>
                <h6>--chef-black</h6>
                <ul>
                  <li>HSL 0, 0%, 0%</li>
                  <li>RGB 0, 0, 0</li>
                  <li>HEX #000000</li>
                </ul>
              </div>
            </div>
          </div>
          <h3>Accessible Color Contrast</h3>
          <h4>What colors can I use for text?</h4>
          <p>As a general rule of thumb, all font colors, when compared to their background colors, should meet WCAG 2.0 AA contrast guidelines.</p>
          <div class="chef-flex-row justify-left">
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-primary-dark"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Primary Dark</strong></h4>
                <h6>--chef-primary-dark</h6>
              </div>
            </div>
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-dark-grey"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Dark Grey</strong></h4>
                <h6>--chef-dark-grey</h6>
              </div>
            </div>
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-primary-bright"></div>
              <div class="chef-card-info">
                <h4><strong>Chef Primary Bright</strong></h4>
                <h6>--chef-primary-bright</h6>
              </div>
            </div>
            <div class="chef-col-2 chef-card-item">
              <div class="chef-color-swatch chef-white"></div>
              <div class="chef-card-info">
                <h4><strong>Chef White</strong></h4>
                <h6>--chef-white</h6>
              </div>
            </div>
          </div>
          <h4>Compliant Color Combinations</h4>
          <p>Color contrast is checked when each atom is designed. If you are working on something where there is no design to follow, please adhere to these combinations to ensure proper WCAG 2.0 AA color contrast.</p>
          <div class="chef-flex-row">
            <div class="chef-col-3 chef-card-item chef-text-base chef-white">
              <div class="chef-card-info">
                <span>Chef Primary Dark<br />on Chef White</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-base chef-lightest-grey">
              <div class="chef-card-info">
                <span>Chef Primary Dark<br />on Chef Lightest Grey</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-base chef-light-grey">
              <div class="chef-card-info">
                <span>Chef Primary Dark<br />on Chef Light Grey</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-base chef-grey">
              <div class="chef-card-info">
                <span>Chef Primary Dark<br />on Chef Grey</span>
              </div>
            </div>
          </div>
          <div class="chef-flex-row justify-left">
            <div class="chef-col-3 chef-card-item chef-text-dull chef-white">
              <div class="chef-card-info">
                <span>Chef Dark Grey<br />on Chef White</span>
              </div>
            </div>
          </div>
          <div class="chef-flex-row justify-left">
            <div class="chef-col-3 chef-card-item chef-text-active chef-white">
              <div class="chef-card-info">
                <span>Chef Primary Bright<br />on Chef White</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-active chef-lightest-grey">
              <div class="chef-card-info">
                <span>Chef Primary Bright<br />on Chef Lightest Grey</span>
              </div>
            </div>
          </div>
          <div class="chef-flex-row justify-left">
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-primary-bright">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Primary Bright</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-dark-grey">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Dark Grey</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-primary-dark">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Primary Dark</span>
              </div>
            </div>
          </div>
          <div class="chef-flex-row justify-left">
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-critical">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Critical</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-major">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Major</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-minor">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Minor</span>
              </div>
            </div>
            <div class="chef-col-3 chef-card-item chef-text-contrast chef-success">
              <div class="chef-card-info">
                <span>Chef White<br />on Chef Success</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }

}
