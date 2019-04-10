import { E2EElement, E2EPage, newE2EPage } from '@stencil/core/testing';

describe('chef-alert', () => {
  let page: E2EPage;
  let element: E2EElement;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`
      <chef-alert type="success">
        Alert Content
      <chef-alert>
    `);
    element = await page.find('chef-alert');
  });

  it('renders', async () => {
    expect(element).toHaveClass('hydrated');
  });

  it('has an `alert` aria role', async () => {
    expect(element.getAttribute('role')).toEqual('alert');
  });

  it('shows the correct icon for success alerts', async () => {
    const icon = await element.find('chef-icon');
    expect(icon.textContent).toEqual('check_circle');
  });
});
