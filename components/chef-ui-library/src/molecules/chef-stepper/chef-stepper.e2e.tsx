import { newE2EPage } from '@stencil/core/testing';

describe('chef-stepper', () => {
  it('displays slotted steps', async () => {
    const html = `
      <chef-stepper>
        <chef-step>Step 1</chef-step>
        <chef-step selected>Step 2</chef-step>
        <chef-step disabled>Step 3</chef-step>
      </chef-stepper>`;

    const page = await newE2EPage();
    await page.setContent(html);

    await page.find('chef-stepper');
    const steps = await page.find('chef-step[disabled]');
    expect(steps.textContent).toEqual(`Step 3`);
  });
});
