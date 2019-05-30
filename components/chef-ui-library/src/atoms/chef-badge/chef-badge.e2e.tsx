import { E2EElement, E2EPage, newE2EPage } from '@stencil/core/testing';

describe('chef-badge', () => {
  let page: E2EPage;
  let element: E2EElement;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`
      <chef-badge>
        <chef-icon>check</chef-icon>
        <span>Badge Text</span>
      </chef-badge>
    `);
    element = await page.find('chef-badge');
  });

  it('renders', async () => {
    expect(element).toHaveClass('hydrated');
  });

  it('displays slotted content', async () => {
    expect(await element.find('chef-icon')).toBeTruthy();
  });

  describe('when `type` prop is not set', () => {
    it('defaults `type` attr to `badge`', async () => {
      expect(element.getAttribute('type')).toEqual('badge');
    });
  });

  describe('when `type` prop is set', () => {
    it('sets `type` attr to prop val', async () => {
      element.setProperty('type', 'reset');
      await page.waitForChanges();

      expect(element.getAttribute('type')).toEqual('reset');
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
    beforeEach(async () => {
      await page.$eval('chef-badge', el => {
        el.addEventListener('click', () => el.innerHTML = 'clicked');
      });
      await page.waitForChanges();
    });

    it('triggers click handlers', async () => {
      await element.click();
      await page.waitForChanges();
      expect(element).toEqualText('clicked');
    });

    describe('and when disabled', () => {
      beforeEach(async () => {
        element.setProperty('disabled', true);
        await page.waitForChanges();
      });

      it('does not trigger click handlers', async () => {
        await element.click();
        await page.waitForChanges();
        expect(element).not.toEqualText('clicked');
      });
    });
  });
});
