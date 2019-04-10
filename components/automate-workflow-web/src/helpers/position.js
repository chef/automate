export default {
  offsetTop: function (el) {
    return el.offsetTop + ( el.offsetParent ? this.offsetTop(el.offsetParent) : 0 );
  },
  offsetLeft: function (el) {
    return el.offsetLeft + ( el.offsetParent ? this.offsetLeft(el.offsetParent) : 0 );
  },
  hasFixedParent: function (el) {
    if (el.nodeName === 'BODY') return false;

    if (getComputedStyle(el).getPropertyValue('position') === 'fixed') {
      return true;
    } else {
      return this.hasFixedParent(el.parentElement);
    }
  }
};
