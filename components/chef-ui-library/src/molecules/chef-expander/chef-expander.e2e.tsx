import { newE2EPage } from '@stencil/core/testing';

describe('chef-expander', () => {
  const html = `
    <chef-expander>
      <p>Some content</p>
    </chef-expander>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-expander');
    expect(element).toHaveClass('hydrated');
  });

  it('displays slotted content', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('p');
    expect(element).toBeDefined();
  });

  describe('open()', () => {
    it('sets `opened` prop to `true`', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-expander');
      element.setProperty('opened', true);

      await page.waitForChanges();

      expect(element.hasAttribute('opened')).toEqual(true);
    });
  });

  describe('close()', () => {
    it('sets `opened` prop to `false`', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-expander');
      element.setProperty('opened', false);

      await page.waitForChanges();

      expect(element.hasAttribute('opened')).toEqual(false);
    });
  });

  describe('toggle()', () => {
    it('toggles `opened` prop', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const element = await page.find('chef-expander');
      element.setProperty('opened', true);

      await element.callMethod('toggle');
      await page.waitForChanges();
      expect(element.hasAttribute('opened')).toEqual(false);

      await element.callMethod('toggle');
      await page.waitForChanges();
      expect(element.hasAttribute('opened')).toEqual(true);
    });
  });
});
