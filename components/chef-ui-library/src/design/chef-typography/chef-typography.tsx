import { Component } from '@stencil/core';

@Component({
  tag: 'chef-typography',
  styleUrl: 'chef-typography.scss'
})
export class ChefTypography {

  render() {
    return (
      <div>
        <h2>Typography Guidelines</h2>
        <p>A typographic scale is used to achieve consistency by minimizing the number of font styles applied throughout Chef's products.</p>

        <h3>Guidelines</h3>
        <h4>Which font size should I use?</h4>
        <p>Page titles (H2) use the 24pt font size. Subtitles and any other headings below the main page title should use decreasing sizes of 18pt (H3) or 14pt (H4). The 36pt (H1) title size should only be used in areas with ample amounts of whitespace, such as empty page states.</p>
        <p>Avoid skipping intermediate font sizes whenever possible, in other words, a page should not have only 24pt and 14pt headings. In cases where lower level headings are needed (H5-H6), you can use alternate weights, colors, or all caps for the title font without going below the minimum 14pt font size.</p>
        <p>To maintain readability, paragraph text should generally not be smaller than 14pt. In cases where smaller text is necessary, such as form labels and tips, 12pt can be used. Font sizes should never be smaller than 12pt.</p>

        <h3>When should I make text bold or italic?</h3>
        <p>Aside from headings, heavier font weights should be used sparingly for greatest impact and are most effective in sections containing a lot of text elements. Similarly, italicized fonts should be employed where CHEF terminology is used or when there is a sense of urgency or warning in the message.</p>

        <h3>Type Scale</h3>
        <div class="chef-flex-wrapper">
          <h4>Headings</h4>
          <div class="chef-flex-row">
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <h1>All your node are belong to us.</h1>
                <h4>h1</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 36px;</li>
                  <li>font-weight: 400;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <h2>All your node are belong to us.</h2>
                <h4>h2</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 24px;</li>
                  <li>font-weight: 400;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <h3>All your node are belong to us.</h3>
                <h4>h3</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 18px;</li>
                  <li>font-weight: 400;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <h4>All your node are belong to us.</h4>
                <h4>h4</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 14px;</li>
                  <li>font-weight: 400;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
          </div>
          <h4>Body</h4>
          <div class="chef-flex-row">
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <p class="chef-font-lg">All your node are belong to us.</p>
                <h4>body (base)</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 16px;</li>
                  <li>font-weight: 300;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <p>All your node are belong to us.</p>
                <h4>body (default)</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 14px;</li>
                  <li>font-weight: 300;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
            <div class="chef-col-8 chef-card-item">
              <div class="chef-card-info">
                <p class="chef-font-sm">All your node are belong to us.</p>
                <h4>body (small)</h4>
              </div>
            </div>
            <div class="chef-col-4 chef-card-item chef-lightest-grey">
              <div class="chef-card-info">
                <ul>
                  <li>font-size: 12px;</li>
                  <li>font-weight: 300;</li>
                  <li>line-height: 1.25em;</li>
                  <li>letter-spacing: normal;</li>
                </ul>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }

}
