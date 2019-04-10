import { newE2EPage } from '@stencil/core/testing';

describe('chef-step', () => {

  const html = `
    <chef-step>
      <chef-icon>check</chef-icon>
    </chef-step>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-step');
    expect(element).toHaveClass('hydrated');
  });

  it('displays slotted content', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-icon');
    expect(element).toBeDefined();
  });

  describe('when `disabled` prop is set to true', () => {
    it('sets `disabled` attribute', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-step');
      element.setProperty('disabled', true);

      await page.waitForChanges();

      expect(element.hasAttribute('disabled')).toEqual(true);
    });
  });

  describe('when `disabled` prop is set to false', () => {
    it('sets `disabled` attribute', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-step');
      element.setProperty('disabled', false);

      await page.waitForChanges();

      expect(element.hasAttribute('disabled')).toEqual(false);
    });
  });

  describe('when `selected` prop is set to true', () => {
    it('sets `selected` attribute', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-step');
      element.setProperty('selected', true);

      await page.waitForChanges();

      expect(element.hasAttribute('selected')).toEqual(true);
    });
  });

  describe('when `selected` prop is set to false', () => {
    it('sets `selected` attribute', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-step');
      element.setProperty('selected', false);

      await page.waitForChanges();

      expect(element.hasAttribute('selected')).toEqual(false);
    });
  });
});
