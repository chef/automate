var popUpWrapper = document.querySelector('.popup-wrapper');
var acceptButton = document.querySelector('.accept-button');

acceptButton.addEventListener('click', closePopup);

function closePopup() {
  popUpWrapper.classList.remove('open');
}