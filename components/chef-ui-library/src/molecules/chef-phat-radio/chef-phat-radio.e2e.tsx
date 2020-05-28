import { newE2EPage } from '@stencil/core/testing';

describe('chef-phat-radio', () => {

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent('<chef-phat-radio><chef-option><chef-option></chef-phat-radio>');
    const el = await page.find('chef-phat-radio');

    expect(el).toBeDefined();
  });

  describe('default initial selection', () => {
    const html = `
      <chef-phat-radio>
        <chef-option value='opt1'>Option 1</chef-option>
        <chef-option value='opt2'>Option 2</chef-option>
      </chef-phat-radio
    `;

    it('selects the first option', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const options = await page.findAll('chef-option');
      const selected = await options[0].getProperty('selected');

      expect(selected).toEqual(true);
    });

    it('sets value to the value of the first option', async () => {
      const page = await newE2EPage();

      await page.setContent(html);
      const options = await page.findAll('chef-option');
      const element = await page.find('chef-phat-radio');

      expect(await element.getProperty('value')).toEqual(await options[0].getProperty('value'));
    });

  });

  describe('initial selection', () => {

    it('selects the proper option', async () => {
      const html = `
        <chef-phat-radio value='opt2'>
          <chef-option value='opt1'>Option 1</chef-option>
          <chef-option value='opt2'>Option 2</chef-option>
          <chef-option value='opt3'>Option 3</chef-option>
        </chef-phat-radio
      `;

      const page = await newE2EPage();

      await page.setContent(html);
      const options = await page.findAll('chef-option');
      const selected = await options[1].getProperty('selected');

      expect(selected).toBeTruthy();
    });

    it('selects the first option if the specified value does not exist', async () => {
      const html = `
        <chef-phat-radio value='opt4'>
          <chef-option value='opt1'>Option 1</chef-option>
          <chef-option value='opt2'>Option 2</chef-option>
          <chef-option value='opt3'>Option 3</chef-option>
        </chef-phat-radio
      `;

      const page = await newE2EPage();

      await page.setContent(html);
      const options = await page.findAll('chef-option');
      const selected = await options[0].getProperty('selected');

      expect(selected).toBeTruthy();
    });

  });

  describe('selection', () => {
    let html, page;

    beforeEach(async () => {
      html = `
        <chef-phat-radio>
          <chef-option value='opt1'>Option 1</chef-option>
          <chef-option value='opt2'>Option 2</chef-option>
          <chef-option value='opt3'>Option 3</chef-option>
        </chef-phat-radio
      `;
      page = await newE2EPage();
      await page.setContent(html);
    });

    it('only the selected option is marked as selected', async () => {
      const options = await page.findAll('chef-option');

      expect(await options[0].getProperty('selected')).toBeTruthy();
      expect(await options[1].getProperty('selected')).not.toBeTruthy();
      expect(await options[2].getProperty('selected')).not.toBeTruthy();
    });

    it('marks only the selected option as selected', async () => {
      const element = await page.find('chef-phat-radio');
      element.setProperty('value', 'opt2');

      await page.waitForChanges();

      const options = await page.findAll('chef-option');

      expect(await options[0].getProperty('selected')).not.toBeTruthy();
      expect(await options[1].getProperty('selected')).toBeTruthy();
      expect(await options[2].getProperty('selected')).not.toBeTruthy();
    });

  });

  describe('when group is deselectable by canDeselect prop', () => {
    let html, page;
    beforeEach(async () => {
      html = `
        <chef-phat-radio deselectable>
          <chef-option value='opt1'>Option 1</chef-option>
          <chef-option value='opt2'>Option 2</chef-option>
          <chef-option value='opt3'>Option 3</chef-option>
        </chef-phat-radio
      `;
      page = await newE2EPage();
      await page.setContent(html);
    });

    it('does not select any options by default', async () => {
      const options = await page.findAll('chef-option');

      expect(await options[0].getProperty('selected')).not.toBeTruthy();
      expect(await options[1].getProperty('selected')).not.toBeTruthy();
      expect(await options[2].getProperty('selected')).not.toBeTruthy();
    });

    it('marks only the selected option as selected', async () => {
      const element = await page.find('chef-phat-radio');
      element.setProperty('value', 'opt2');

      await page.waitForChanges();

      const options = await page.findAll('chef-option');

      expect(await options[0].getProperty('selected')).not.toBeTruthy();
      expect(await options[1].getProperty('selected')).toBeTruthy();
      expect(await options[2].getProperty('selected')).not.toBeTruthy();
    });
  });

});
