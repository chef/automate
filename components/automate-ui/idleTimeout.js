
function idleLogout() {
      var idleTime = 0;
      // Increment the idle time counter 3 sec.
      var idleInterval = setInterval(timerIncrement, 3000); // 3 sec
      window.onload = resetTimer;
      window.onmousemove = resetTimer;
      window.onmousedown = resetTimer;  // catches touchscreen presses as well
      window.ontouchstart = resetTimer; // catches touchscreen swipes as well
      window.onclick = resetTimer;      // catches touchpad clicks as well
      window.onkeydown = resetTimer;
      window.addEventListener('scroll', resetTimer, true);


    function resetTimer() {
      idleTime = 0;
    }

    function timerIncrement() {
      idleTime = idleTime + 1;
      if (idleTime > 2) { // 6 sec
          console.log('logout')
          // window.location.reload();
      }
    }
}
idleLogout();

window.onload = function() {
  idleLogout();
}
