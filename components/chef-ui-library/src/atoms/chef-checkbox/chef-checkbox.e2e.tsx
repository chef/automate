import { E2EElement, E2EPage, newE2EPage } from '@stencil/core/testing';

describe('chef-checkbox', () => {
  let page: E2EPage;
  let element: E2EElement;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`
      <chef-checkbox></chef-checkbox>
    `);
    element = await page.find('chef-checkbox');
  });

  it('renders', async () => {
    expect(element).toHaveClass('hydrated');
  });

  it('sets `role` attribute to `checkbox`', async () => {
    expect(element.getAttribute('role')).toEqual('checkbox');
  });

  describe('when `checked` property is false', () => {
    beforeEach(async () => {
      element.setProperty('checked', false);
      await page.waitForChanges();
    });

    it('sets `aria-checked` attribute to `false`', async () => {
      expect(element.getAttribute('aria-checked')).toEqual('false');
    });

    it('displays `unchecked` icon', async () => {
      const icon = await element.find('chef-icon');
      expect(icon).toEqualText('close');
    });
  });

  describe('when `checked` property is true', () => {
    beforeEach(async () => {
      element.setProperty('checked', true);
      await page.waitForChanges();
    });

    it('sets `aria-checked` attribute to `true`', async () => {
      expect(element.getAttribute('aria-checked')).toEqual('true');
    });

    it('displays `checked` icon', async () => {
      const icon = await element.find('chef-icon');
      expect(icon).toEqualText('check');
    });
  });

  describe('when `indeterminate` property is false', () => {
    beforeEach(async () => {
      element.setProperty('indeterminate', false);
      await page.waitForChanges();
    });

    it('sets `aria-checked` attribute to value of `checked` property', async () => {
      element.setProperty('checked', false);
      await page.waitForChanges();
      expect(element.getAttribute('aria-checked')).toEqual('false');

      element.setProperty('checked', true);
      await page.waitForChanges();
      expect(element.getAttribute('aria-checked')).toEqual('true');
    });

    it('displays icon for value of `checked` property', async () => {
      const icon = await element.find('chef-icon');

      element.setProperty('checked', false);
      await page.waitForChanges();
      expect(icon).toEqualText('close');

      element.setProperty('checked', true);
      await page.waitForChanges();
      expect(icon).toEqualText('check');
    });
  });

  describe('when `indeterminate` property is true', () => {
    beforeEach(async () => {
      element.setProperty('indeterminate', true);
      await page.waitForChanges();
    });

    it('sets `aria-checked` attribute to `mixed`', async () => {
      expect(element.getAttribute('aria-checked')).toEqual('mixed');
    });

    it('displays `indeterminate` icon', async () => {
      const icon = await element.find('chef-icon');
      expect(icon).toEqualText('remove');
    });
  });

  describe('when `disabled` property is false', () => {
    beforeEach(async () => {
      element.setProperty('disabled', false);
      await page.waitForChanges();
    });

    it('does not have an `aria-disabled` attribute', async () => {
      expect(element.hasAttribute('aria-disabled')).toEqual(false);
    });
  });

  describe('when `disabled` property is true', () => {
    beforeEach(async () => {
      element.setProperty('disabled', true);
      await page.waitForChanges();
    });

    it('has an `aria-disabled` attribute', async () => {
      expect(element.hasAttribute('aria-disabled')).toEqual(true);
    });
  });

  describe('when clicked', () => {
    it('toggles value of `checked` property', async () => {
      element.setProperty('checked', false);
      await page.waitForChanges();

      await element.click();
      await page.waitForChanges();
      expect(await element.getProperty('checked')).toEqual(true);

      await element.click();
      await page.waitForChanges();
      expect(await element.getProperty('checked')).toEqual(false);
    });

    it('resets value of `indeterminate` property to false', async () => {
      element.setProperty('indeterminate', true);
      await page.waitForChanges();

      await element.click();
      await page.waitForChanges();
      expect(await element.getProperty('indeterminate')).toEqual(false);
    });

    describe('and when disabled', () => {
      beforeEach(async () => {
        element.setProperty('disabled', true);
        await page.waitForChanges();
      });

      it('does not change `checked` and `indeterminate` properties', async () => {
        element.setProperty('checked', false);
        element.setProperty('indeterminate', true);
        await page.waitForChanges();

        await element.click();
        await page.waitForChanges();
        expect(await element.getProperty('checked')).toEqual(false);
        expect(await element.getProperty('indeterminate')).toEqual(true);
      });
    });
  });

  describe('when focused and spacebar is pressed', () => {
    it('toggles value of `checked` property', async () => {
      element.setProperty('checked', false);
      await page.waitForChanges();

      await element.focus();
      await element.press('Space');
      await page.waitForChanges();
      expect(await element.getProperty('checked')).toEqual(true);

      await element.press('Space');
      await page.waitForChanges();
      expect(await element.getProperty('checked')).toEqual(false);
    });

    it('resets value of `indeterminate` property to false', async () => {
      element.setProperty('indeterminate', true);
      await page.waitForChanges();

      await element.focus();
      await element.press('Space');
      await page.waitForChanges();
      expect(await element.getProperty('indeterminate')).toEqual(false);
    });
  });
});
