import { newE2EPage } from '@stencil/core/testing';

describe('chef-loading-spinner', () => {

  const html = `<chef-loading-spinner></chef-loading-spinner>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-loading-spinner');
    expect(element).toHaveClass('hydrated');
  });

  it('displays a loading spinner', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('svg[role="status"]');
    expect(element).toBeTruthy();
  });

  describe('when `fixed` prop is true', () => {
    it('has a `fixed` attribute', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-loading-spinner');

      element.setProperty('fixed', true);
      await page.waitForChanges();

      expect(element.hasAttribute('fixed')).toEqual(true);
    });
  });

  describe('when `fixed` prop is false', () => {
    it('does not have a `fixed` attribute', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-loading-spinner');

      element.setProperty('fixed', false);
      await page.waitForChanges();

      expect(element.hasAttribute('fixed')).toEqual(false);
    });
  });
});
